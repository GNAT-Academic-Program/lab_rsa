with Ada.Text_IO;
with GNAT.Sockets;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Server_Main is

   Tasks_To_Create : constant := 2;

   Terminator : constant Character := ASCII.NUL;

   type Integer_List is array (1 .. Tasks_To_Create) of Integer;
   subtype Counter is Integer range 0 .. Tasks_To_Create;
   subtype Index is Integer range 1 .. Tasks_To_Create;

   subtype UString is Ada.Strings.Unbounded.Unbounded_String;

   protected type Messages is
      procedure Add (New_Char : Character);
      procedure Get (Msg : out UString);
   private
      M : UString;
   end Messages;

   type Messages_List is array (1 .. Tasks_To_Create) of Messages;

   protected body Messages is
      procedure Add (New_Char : Character) is
      begin
         Append (M, New_Char);
      end Add;

      procedure Get (Msg : out UString) is
      begin
         Msg := M;
         Delete (M, Positive'First, Length (M));
      end Get;
   end Messages;

   protected type Info is
      procedure Push_Stack (Return_Task_Index : Index);
      procedure Initialize_Stack;
      function Number_Of_Tasks return Counter;
      entry Pop_Stack (Get_Task_Index : out Index);
   private
      Task_Stack    : Integer_List;
      Stack_Pointer : Counter := 0;
   end Info;

   protected body Info is
      procedure Push_Stack (Return_Task_Index : Index) is
      begin
         Stack_Pointer              := Stack_Pointer + 1;
         Task_Stack (Stack_Pointer) := Return_Task_Index;
      end Push_Stack;

      entry Pop_Stack (Get_Task_Index : out Index) when Stack_Pointer /= 0 is
      begin
         Get_Task_Index := Task_Stack (Stack_Pointer);
         Stack_Pointer  := Stack_Pointer - 1;
      end Pop_Stack;

      procedure Initialize_Stack is
      begin
         for I in Task_Stack'Range loop
            Push_Stack (I);
         end loop;
      end Initialize_Stack;

      function Number_Of_Tasks return Counter is (Stack_Pointer);
   end Info;

   Task_Info     : Info;
   Task_Messages : Messages_List;

   task type SocketTask is
      entry Setup
        (Connection : GNAT.Sockets.Socket_Type;
         Client     : GNAT.Sockets.Sock_Addr_Type;
         Channel    : GNAT.Sockets.Stream_Access; Task_Index : Index);
      entry Read_Write;
   end SocketTask;

   task body SocketTask is
      my_Connection : GNAT.Sockets.Socket_Type;
      my_Client     : GNAT.Sockets.Sock_Addr_Type;
      my_Channel    : GNAT.Sockets.Stream_Access;
      my_Index      : Index;
      my_Message    : UString;

      task type Write is
         entry Start;
      end Write;

      task body Write is
      begin
         accept Start;
         loop
            delay 0.1;
            Task_Messages (my_Index).Get (my_Message);
            if Length (my_Message) /= 0 then
               String'Write (my_Channel, To_String (my_Message));
               Delete (my_Message, Positive'First, Length (my_Message));
            end if;
         end loop;
      end Write;

      W : Write;

      task type Read is
         entry Start;
      end Read;

      task body Read is
         procedure Handle_Read is
         begin
            declare
               C : constant Character := Character'Input (my_Channel);
            begin
               if C = Terminator then
                  Ada.Text_IO.New_Line;
               else
                  Ada.Text_IO.Put (C);
               end if;
               Task_Messages (if my_Index = 1 then 2 else 1).Add (C);
            end;
         exception
            when others =>
               GNAT.Sockets.Close_Socket (my_Connection);
               Task_Info.Push_Stack (my_Index);
               Ada.Text_IO.Put_Line
                 ("Task " & Integer'Image (my_Index) & " Cleaned Up");
         end Handle_Read;
      begin
         accept Start;
         loop
            Handle_Read;
         end loop;
      end Read;

      R : Read;
   begin
      loop
         accept Setup
           (Connection : GNAT.Sockets.Socket_Type;
            Client     : GNAT.Sockets.Sock_Addr_Type;
            Channel    : GNAT.Sockets.Stream_Access; Task_Index : Index)
         do
            my_Connection := Connection;
            my_Client     := Client;
            my_Channel    := Channel;
            my_Index      := Task_Index;
         end Setup;

         accept Read_Write;
         begin
            Ada.Text_IO.Put_Line ("Task " & Integer'Image (my_Index));
            R.Start;
            W.Start;
         end;
      end loop;
   end SocketTask;

   task type SocketServer (my_Port : GNAT.Sockets.Port_Type) is
      entry Listen;
   end SocketServer;

   task body SocketServer is
      Receiver   : GNAT.Sockets.Socket_Type;
      Connection : GNAT.Sockets.Socket_Type;
      Client     : GNAT.Sockets.Sock_Addr_Type;
      Channel    : GNAT.Sockets.Stream_Access;
      Worker     : array (1 .. Tasks_To_Create) of SocketTask;
      Use_Task   : Index;

   begin
      accept Listen;
      GNAT.Sockets.Create_Socket (Socket => Receiver);
      GNAT.Sockets.Set_Socket_Option
        (Socket => Receiver, Level => GNAT.Sockets.Socket_Level,
         Option => (Name => GNAT.Sockets.Reuse_Address, Enabled => True));
      GNAT.Sockets.Bind_Socket
        (Socket  => Receiver,
         Address =>
           (Family => GNAT.Sockets.Family_Inet,
             Addr  => GNAT.Sockets.Inet_Addr ("127.0.0.1"), Port => my_Port));
      GNAT.Sockets.Listen_Socket (Socket => Receiver);
      Task_Info.Initialize_Stack;

      Find :
      loop -- Block for connection and take next free task.
         GNAT.Sockets.Accept_Socket
           (Server => Receiver, Socket => Connection, Address => Client);

         Ada.Text_IO.Put_Line ("Connect " & GNAT.Sockets.Image (Client));

         Channel := GNAT.Sockets.Stream (Connection);

         Task_Info.Pop_Stack (Use_Task);
         --  Protected guard waits if full house.

         --  Setup the socket in this task in rendezvous.
         Worker (Use_Task).Setup (Connection, Client, Channel, Use_Task);

         --  Run the asynchronous task for the socket communications.
         Worker (Use_Task).Read_Write; --  Start Read and Write.

      end loop Find;
   end SocketServer;
   Echo_Server : SocketServer (my_Port => 12_321);
begin
   Echo_Server.Listen;
end Server_Main;
