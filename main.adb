with BoundedQueues;
with RDP;
with Ada.Text_IO; use Ada.Text_IO;
--type QueueIndex is range 0 .. 1_024;

--procedure Send (channel: out Channel_Type; conn: in out Connection) is
--begin

--end Send;

procedure main is

   package R is new RDP (Max => 1024);
   use R;

   LOCAL_PORT  : Port_Type                  := 1_024;
   REMOTE_PORT : Port_Type                  := 1_024;
   snd_max     : Sequence_Number_Type       := 1_024; -- arbitrarily chosen
   rbuf_max    : Segment_Size_In_Bytes_Type := 64; -- arbitrarily chosen;
   Alice: Connection ;
   Bob :  Connection ;
   channel : Channel_Type.Queue ;
begin
   -- make
   Alice:= Create_Connection;
   Bob := Create_Connection;
   channel := Channel_Type.Make;
   
   --open alice
   pragma Assert (channel.First = 0);
   Open_Connection
     (conn    => Alice, channel => channel, active_request => False,
      snd_max => snd_max, rmax_buf => rbuf_max);
   pragma Assert (Channel_Type.Consistent(channel));

   --open bob
   Open_Connection
     (conn    => Bob, channel => channel, active_request => True,
      snd_max => snd_max, rmax_buf => rbuf_max);
   
   --send a bunch of stuff
   for I in 0..4 loop
      pragma Loop_Invariant(channel.Size = I+1);
      Send_To_Connection(conn    => Alice,
                         channel => channel,
                         data    => (others => False));
   end loop;

   while not Channel_Type.IsEmpty(channel) loop
      Rcv_From_Connection(conn => Bob, channel => channel);
   end loop;
   
end main;
