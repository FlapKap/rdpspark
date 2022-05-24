with BoundedQueues;
generic
   Max : Positive;
package RDP is
   pragma Pure;
   pragma Preelaborate;

   BUF_SIZE : constant Natural := 1_024;

   --## basic types
   type Sequence_Number_Type is
     mod 2**32; -- mod type does not check for overflow in arithmetic!

   type Segment_Size_In_Bytes_Type is new Natural range 0 .. (2**8 - 1);

   type Ack_Segments_Out_Of_Sequence_Array_Type is
     array (Sequence_Number_Type) of Sequence_Number_Type;
   type Data_Array_Type is array (0 .. BUF_SIZE) of Boolean; -- TODO fix this

   type Data_Length_Type is new Natural range 0 .. (2**16 - 1);
   type Header_Length_Type is new Natural range 0 .. (2**8 - 1);
   type State_Type is (OPEN, CLOSED, LISTEN, SYN_RCVD, SYN_SENT, CLOSE_WAIT);
   type Checksum_Type is mod 2**32;
   type Port_Type is
     new Natural range 0 .. (2**16 - 1); --changed in updated spec (RFC 1151)

   type Control_Flags_Type is (SYN, ACK, EACK, RST, NUL);
   --Spec includes Zero flag that is unused

   -- SEGMENT Types
   -- ACK can accompany any message
   -- SYN and RST are sent as separate segments and must not include data
   -- NUL must have Zero data length, but can be sent with ACK and EACK
   -- Zero is unused and must therefore be zero

   type Flags_Type is record
      SYN    : Boolean := False;
      ACK    : Boolean := False;
      EACK   : Boolean := False;
      RST    : Boolean := False;
      NUL    : Boolean := False;
      Unused : Boolean := False;
   end record;

   --  -- this bitordering is probably wrong
   --  for Flags_Type use record
   --    SYN    at 0 range 0 .. 0;
   --    ACK    at 0 range 1 .. 1;
   --    EACK   at 0 range 2 .. 2;
   --    RST    at 0 range 3 .. 3;
   --    NUL    at 0 range 4 .. 4;
   --    Unused at 0 range 5 .. 5;
   --  end record; -- possibly force endianness here

   --have to have defaults for everything for spark to be happy
   type Header
      --(Kind : Control_Flags_Type := ACK)--(len : Natural)
      is record -- fix range of len. Should be more restrictive than natural
      Flags : Flags_Type := (others => False);
      -- Flags   : array (Control_Flags) of Boolean;
      Version : Natural range 0 .. 2**2 := 2; --updated in RFC 1151

      Header_Length    : Header_Length_Type := 255; --unused in this
      Source_Port      : Port_Type          := 255;
      Destination_Port : Port_Type          := 255;

      Data_Length : Data_Length_Type     := Data_Length_Type (BUF_SIZE);
      Seq_Number  : Sequence_Number_Type := 100;
      Ack_Number  : Sequence_Number_Type := 1;

      Checksum : Checksum_Type   := 1;
      Data     : Data_Array_Type := (others => False);
      --  case Kind is
      --     when SYN =>
      Max_Outstanding_Segments : Sequence_Number_Type       := 1_024;
      Max_Segment_Size         : Segment_Size_In_Bytes_Type := 64;
      SDM                      : Boolean                    := False;
      --        -- in spec is options flag field, but only contains one
      --     when EACK =>
      Sequences_RCVd_OK : Ack_Segments_Out_Of_Sequence_Array_Type :=
        (others => 0);
      --     when ACK | RST | NUL =>
      --        -- RST and NUL are not supposed to hold data
      --        null;
      --  end case;

   end record;

   -- # overview of segment types
   -- ## SYN
   -- Used to establish connection and synchronize sequence numbers and just sync info
   --
   -- ACK number should only be set if ACK flag is set
   -- Variable area has 3 fields - all 2 bytes
   -- - Max # of Outstanding Segments
   -- - Max segment size
   -- - Options flag field
   --   - Only 1 flag on bit 0. Sequence delivery mode (SDM)
   --     if set (1) requires that segments are delivered to user in order, while unset (0) delivers in arrival order

   -- ## ACK
   -- may be sent as separate segment but should be combined with data is possible
   -- Must always have the ACK bit set and the ack number field
   -- Data length must be non-zero if it includes data
   -- Advance sequence number of data is sent. If no data is sent we dont use this field
   -- Ack number field contains last ackd sequence number received in sequential order. If not recieved sequentially use EACK

   -- ## EACK
   -- used to ack segments recieved out of order
   -- Always also include the ACK segment of the last in sequence segment recieved
   -- may include data
   -- Advance sequence number of data is sent. If no data is sent we dont use this field
   -- Ack number field contains last ackd sequence number received in sequential order
   -- Then it contains a series of Sequence numbers that are recieved OK
   -- and then data if included
   --
   -- This must mean that you only know where the sequence numbers end by, if no data is sent, just reading after the checksum
   -- and if data is sent then subtract data length from total segment length

   -- ## RST
   -- used to close or reset connection
   -- after recieving the sender must stop sending more and handling unserviced request
   -- Data length must be zero

   -- ## NUL
   -- used as heartbeat
   -- Data length must be zero
   -- When NUL is recieved it must be acknowledged  if there is a connection and if the provided sequence number falls within the acceptable range
   -- it may be combined with ACK but never with data

   --  package Channel_Type is new BoundedQueues
   --    (Max => 8, Element_Type => Character);

   --function Create_Channel return Channel_Type.Queue;

   function IsSYN (s : Header) return Boolean;
   function IsACK (s : Header) return Boolean;
   function IsEACK (s : Header) return Boolean;
   function IsRST (s : Header) return Boolean;
   function IsNUL (s : Header) return Boolean;
   function isValidSegment (s : Header) return Boolean;

   type Connection is record
      --Channel : Channel_Type.Queue;
      STATE : State_Type;

      LOCAL_PORT  : Port_Type;
      REMOTE_PORT : Port_Type;

      --## SENDER VARIABLES
      SND_NXT : Sequence_Number_Type; -- next seqnum to be sent
      SND_UNA : Sequence_Number_Type; -- oldest unacknowledged segment
      SND_MAX : Sequence_Number_Type; -- max # of unackd segments that can be sent
      SND_ISS : Sequence_Number_Type; -- initial send seq#. was sent in SYN

      --## RECEIVER VARIABLES
      RCV_CUR   : Sequence_Number_Type; -- last segment received correctly
      RCV_MAX : Sequence_Number_Type; -- max amount of segments that can be buffered
      RCV_IRS   : Sequence_Number_Type; -- initial seq#. Established in SYN
      RCVDSEQNO : Ack_Segments_Out_Of_Sequence_Array_Type :=
        (others => 0); --where is this maxlen defined in the paper?

      --## OTHER VARIABLES
      CLOSEWAIT : Natural; --timeout in ms
      SBUF_MAX  : Segment_Size_In_Bytes_Type;
      --max seg size, in bytes, the sender should send. Incl. header.

      RBUF_MAX : Segment_Size_In_Bytes_Type;
      --max seg size, in bytes, that can be received. Incl. header.

      --## CURRENT SEGMENT VARIABLES
      SEG_SEQ  : Sequence_Number_Type; -- Segment currently processed
      SEG_ACK : Sequence_Number_Type; --ack number in current segment processed
      SEG_MAX : Sequence_Number_Type; -- max number of outstanding segments receiver is willing to hold
      SEG_BMAX : Segment_Size_In_Bytes_Type; -- max segsize accepted by foreign host
   end record;

   -- FUNCTIONS

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
      return Connection;

   -- ##Participants
   package Channel_Type is new BoundedQueues
     (Max => Max, Element_Type => Header);
   use Channel_Type;

   --## PROCEDURES
   procedure Open_Connection
     (conn           : in out Connection; channel : in out Queue;
      active_request :        Boolean; snd_max : Sequence_Number_Type;
      rmax_buf       :        Segment_Size_In_Bytes_Type) with
      Pre => conn.STATE = CLOSED
      and then
      ((Consistent (channel) and then isEmpty(channel))
       and then (if active_request then not IsFull (channel))),
      Post =>
      (conn.SND_ISS = conn.SND_UNA and conn.SND_NXT = conn.SND_ISS + 1 and
       conn.SND_MAX = snd_max and conn.RBUF_MAX = rmax_buf)
      and then Consistent (channel)
      and then
      (if active_request then
         (conn.STATE = SYN_SENT and
          GetSize (channel) + 1 = GetSize (channel'Old))
       else
         (conn.STATE = LISTEN and GetSize (channel) = GetSize (channel'Old)));

   procedure Close_Connection
     (conn : in out Connection; channel : in out Queue) with
      Pre =>
      (conn.STATE in OPEN | LISTEN | SYN_RCVD | SYN_SENT and
       conn.SND_NXT < (conn.SND_UNA + conn.SND_MAX))
      and then Consistent (channel) and then IsEmpty (channel),
      Post => conn.STATE in CLOSED | CLOSE_WAIT;

   procedure Send_To_Connection
     (conn : in out Connection; channel : in out Queue;
      data :        Data_Array_Type) with
      Pre =>
      ((conn.STATE = OPEN) and (conn.SND_NXT < conn.SND_UNA + conn.SND_MAX))
      and then Consistent (channel) and then (not IsFull (channel)),
      Post => conn.SND_NXT'Old + 1 =
      conn.SND_NXT --and channel.Sithen Consistent (channel)
      and then not IsEmpty (channel);

   procedure Rcv_From_Connection
     (conn : in out Connection; channel : in out Queue) with
      Pre => Consistent (channel)
      and then
      ((not IsEmpty (channel))
       and then
       (IsRST (GetContent (channel) (GetFirst (channel)).Element) or
        IsACK (GetContent (channel) (GetFirst (channel)).Element) or
        IsNUL (GetContent (channel) (GetFirst (channel)).Element)));

end RDP;
