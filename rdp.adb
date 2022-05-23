with BoundedQueues;

package body RDP is

--    function Create_Channel return Channel_Type.Queue is
-- begin
--     return new
-- end Create_Channel;

   AlreadyOpen           : exception;
   Closing               : exception;
   NotOpen               : exception;
   InsufficientResources : exception;
   NotPossible           : exception;

   function IsSYN (s : Header) return Boolean is
     (s.Flags.SYN = True and s.Flags.EACK = False and s.Flags.RST = False and
      s.Flags.NUL = False and s.Data_Length = 0 and
      s.Max_Outstanding_Segments > 0 and s.Max_Segment_Size > 0);
   function IsACK (s : Header) return Boolean is
     (s.Flags.ACK = True and s.Flags.EACK = False and s.Flags.RST = False and
      s.Flags.NUL = False and s.Seq_Number > 0 and s.Ack_Number > 0);
   function IsEACK (s : Header) return Boolean is
      count : Sequence_Number_Type := 0;
   begin

      --early exist if fields doesnt match
      if
        (not
         (s.Flags.ACK = True and s.Flags.EACK = True and
          s.Flags.RST = False and s.Flags.NUL = False and s.Seq_Number > 0 and
          s.Ack_Number > 0))
      then
         return False;
      end if;

      for i in s.Sequences_RCVd_OK'Range loop
         if s.Sequences_RCVd_OK (i) > 0 then
            count := count + 1;
         end if;
      end loop;

   --this differs from spec, but since I dont use header length, this will do
      return count = Sequence_Number_Type (s.Header_Length);
   end IsEACK;

   function IsRST (s : Header) return Boolean is
     (s.Flags.SYN = False and s.Flags.ACK = False and s.Flags.EACK = False and
      s.Flags.RST = True and s.Flags.NUL = False and s.Data_Length = 0);
   function IsNUL (s : Header) return Boolean is
     (s.Flags.SYN = False and s.Flags.ACK = False and s.Flags.EACK = False and
      s.Flags.RST = False and s.Flags.NUL = True and s.Data_Length = 0);

   function isValidSegment (s : Header) return Boolean is
     (IsSYN (s) or IsACK (s) or IsEACK (s) or IsRST (s) or IsNUL (s));

   function Create_Connection
     (STATE   : State_Type := CLOSED; SND_NXT : Sequence_Number_Type := 1;
      SND_UNA : Sequence_Number_Type := 0;
      SND_MAX : Sequence_Number_Type := 1_024;

      SND_ISS : Sequence_Number_Type := 0;
   -- sent in SYN - but if we are the sender?

   --## RECEIVER VARIABLES

      RCV_MAX : Sequence_Number_Type := 1_024;
   -- arbitrary default

      RCV_IRS : Sequence_Number_Type := 0;
   -- initial seq#. Established in SYN

   --## OTHER VARIABLES

CLOSEWAIT      : Natural                    := 10_000;
      SBUF_MAX : Segment_Size_In_Bytes_Type := 255;
   --max seg size, in bytes, the sender should send. Incl. header.

      RBUF_MAX : Segment_Size_In_Bytes_Type := 255;
   --max seg size, in bytes, that can be received. Incl. header.
-- buffer_len : Natural;

      LOCAL_PORT : Port_Type := 1_024; REMOTE_PORT : Port_Type := 1_024)
      return Connection is
   -- all defaults below should be changed

     (LOCAL_PORT => LOCAL_PORT, REMOTE_PORT => REMOTE_PORT, STATE => STATE,
      SND_NXT    => SND_NXT, SND_UNA => SND_UNA, SND_MAX => SND_MAX,
      SND_ISS    => SND_ISS,
      RCV_CUR    => 0, -- some default
      RCV_MAX    => RCV_MAX, RCV_IRS => RCV_IRS,
      RCVDSEQNO  => <>, --means it takes default value... an ada 2005 feature
      CLOSEWAIT  => CLOSEWAIT, SBUF_MAX => SBUF_MAX,
      RBUF_MAX   => RBUF_MAX,
      SEG_SEQ    => 0, --some default
      SEG_ACK    => 0, --some default
      SEG_MAX    => 1_024,
      SEG_BMAX   =>
        64 -- some default

);

   --Check that connection is closed
   procedure Open_Connection
     (conn           : in out Connection; channel : in out Queue;
      active_request :        Boolean; snd_max : Sequence_Number_Type;
      rmax_buf       :        Segment_Size_In_Bytes_Type)
   is
      remote_port : Port_Type := 255; -- just a random port
      local_port  : Port_Type := 255; -- just a random port
   begin

      if not (conn.STATE = CLOSED) then
         raise AlreadyOpen;

      end if;

      -- can be active or passive open request:
      -- active opens establishes a connection at the same time
      -- passive opens just sets the state to listen

      -- At first lets assume only one connection is possible
      conn.SND_ISS  := 0;
      conn.SND_NXT  := conn.SND_ISS + 1;
      conn.SND_UNA  := conn.SND_ISS;
      conn.SND_MAX  := snd_max;
      conn.RBUF_MAX := rmax_buf;

      if active_request then
         conn.LOCAL_PORT  := local_port;
         conn.REMOTE_PORT := remote_port;
         pragma Assert(not IsFull(Q => channel));
         Push
           (channel,
            (Flags => (SYN => True, others => <>), Source_Port => local_port,
             Destination_Port         => remote_port, Data_Length => 0, --fix
             Seq_Number               => conn.SND_ISS, Ack_Number => 0,
             Max_Outstanding_Segments => conn.SEG_MAX,
             Max_Segment_Size         => conn.RBUF_MAX, others => <>));

         conn.STATE := SYN_SENT;

      else --passive
         conn.LOCAL_PORT := local_port;
         conn.STATE      := LISTEN;
      end if;

   end Open_Connection;

   --
   procedure Close_Connection
     (conn : in out Connection; channel : in out Queue)
   is
   begin
      case conn.STATE is
         when OPEN =>
            Push
              (Q => channel,
               e =>
                 (Flags      => (RST => True, others => False),
                  Seq_Number => conn.SND_NXT, others => <>));
            conn.STATE := CLOSE_WAIT;
         when LISTEN =>
            conn.STATE := CLOSED;
         when SYN_RCVD | SYN_SENT =>
            Push
              (Q => channel,
               e =>
                 (Flags      => (RST => True, others => False),
                  Seq_Number => conn.SND_NXT, others => <>));
            conn.STATE := CLOSED;
         when CLOSE_WAIT =>
            raise Closing;
         when CLOSED =>
            raise NotOpen;
      end case;

   end Close_Connection;

   procedure Send_To_Connection
     (conn : in out Connection; channel : in out Queue; data : Data_Array_Type)
   is
   begin
      --early exits
      if not (conn.STATE = OPEN) then
         raise NotOpen;
      end if;

      if not (conn.SND_NXT < conn.SND_UNA + conn.SND_MAX) then
         raise InsufficientResources;
      end if;

      Push
        (Q => channel,
         e =>
           (Flags            => (ACK => True, others => <>),
            Source_Port      => conn.LOCAL_PORT,
            Destination_Port => conn.REMOTE_PORT,
            Data_Length      => data'Length, --fix
            Seq_Number       => conn.SND_NXT, Ack_Number => conn.RCV_CUR,
            Data             => data, others => <>));

      conn.SND_NXT := conn.SND_NXT + 1;

   end Send_To_Connection;

   procedure Rcv_From_Connection
     (conn : in out Connection; channel : in out Queue)
   is
      s : Header;
   begin
      Pop (Q => channel, e => s);

      case conn.STATE is
         when CLOSED =>
            if IsRST (s) then
               null;
            elsif IsACK (s) or IsNUL (s) then
               Push
                 (Q => channel,
                  e =>
                    (Flags            => (RST => True, others => <>),
                     Source_Port      => conn.LOCAL_PORT,
                     Destination_Port => conn.REMOTE_PORT,
                     Seq_Number       => conn.SEG_ACK + 1, others => <>));
            else
               Push
                 (Q => channel,
                  e =>
                    (Flags            => (RST => True, others => <>),
                     Source_Port      => conn.LOCAL_PORT,
                     Destination_Port => conn.REMOTE_PORT, Seq_Number => 0,
                     ack_Number       => conn.SEG_SEQ, others => <>));
            end if;

         when CLOSE_WAIT =>
            if IsRST (s) then
               conn.STATE := CLOSED;
            end if;
         when LISTEN =>
            if IsRST (s) then
               null;
            elsif IsACK (s) or IsNUL (s) then
               Push
                 (Q => channel,
                  e =>
                    (Flags            => (RST => True, others => <>),
                     Source_Port      => conn.LOCAL_PORT,
                     Destination_Port => conn.REMOTE_PORT,
                     Seq_Number       => conn.SEG_ACK + 1, others => <>));
            elsif IsSYN (s) then
               conn.RCV_CUR  := conn.SEG_SEQ;
               conn.RCV_IRS  := conn.SEG_SEQ;
               conn.SND_MAX  := conn.SEG_MAX;
               conn.SBUF_MAX := conn.SEG_BMAX;
               Push
                 (Q => channel,
                  e =>
                    (Flags => (SYN => True, ACK => True, others => <>),
                     Source_Port              => conn.LOCAL_PORT,
                     Destination_Port         => conn.REMOTE_PORT,
                     Seq_Number => conn.SND_ISS, Ack_Number => conn.RCV_CUR,
                     Max_Outstanding_Segments => conn.RCV_MAX,
                     Max_Segment_Size         => conn.RBUF_MAX, others => <>));

               conn.STATE := SYN_RCVD;
            else
               raise NotPossible;
            end if;

         when SYN_SENT =>
            null; --- not finished
         when others =>
            null;
            --  Pop(Q => channel,
            --      e => s); -- not finished
      end case;
   end Rcv_From_Connection;

end RDP;
