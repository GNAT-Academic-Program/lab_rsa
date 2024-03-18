with GNAT.Sockets; use GNAT.Sockets;
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Streams;  use Ada.Streams;

with RSA;   use RSA;
with Utils; use Utils;

procedure Client_Main is
   Client  : Socket_Type;
   Channel : Stream_Access;

   Partner_Pub_Key_E : Integer := 0;
   Partner_Pub_Key_N : Integer := 0;

   Terminator : constant Character := ASCII.NUL;

   task Read_Received is
      entry Start;
   end Read_Received;

   task body Read_Received is
      function Get_Message return String is
         Offset : Stream_Element_Count;
         Data   : Stream_Element_Array (1 .. 1);
         C      : Character;
      begin
         Read (Channel.all, Data, Offset);
         C := Character'Val (Data (Data'First));
         if C = Terminator then
            return "";
         else
            return C & Get_Message;
         end if;
      end Get_Message;

      function Filter_Message return String is
         Msg : constant String := Get_Message;
         procedure Extract_Key is
         begin
            for C in Msg'Range loop
               if Msg (C) = ',' then
                  Partner_Pub_Key_E := Integer'Value (Msg (8 .. C - 1));
                  Partner_Pub_Key_N := Integer'Value (Msg (C + 1 .. Msg'Last));
                  exit;
               end if;
            end loop;
         end Extract_Key;
      begin
         if Msg'Length > 7 and then Msg (1 .. 7) = "PubKey:" then
            Extract_Key;
            Put_Line
              (":> Received Partner Public Key: " & "(" &
               Partner_Pub_Key_E'Image & "," & Partner_Pub_Key_N'Image & ")");
         else
            Put_Line (":> Received: " & Msg);
            Put_Line (":> Decrypted: " & Decrypt_Msg (Msg));
         end if;
         return Msg;
      end Filter_Message;

   begin
      accept Start;
      loop
         delay 0.1;
         declare
            Msg : constant String := Filter_Message;
         begin
            exit when Msg = "quit";
         end;
      end loop;
   end Read_Received;

   procedure Send_Msg
     (Ch : Stream_Access; Msg : String; Encrypted : Boolean := False)
   is
   begin
      if Encrypted then
         String'Write
           (Ch,
            Encrypt_Msg (Msg, Partner_Pub_Key_E, Partner_Pub_Key_N) &
            Terminator);
      else
         String'Write (Ch, Msg & Terminator);
      end if;
   end Send_Msg;

begin
   Create_Socket (Socket => Client);
   Connect_Socket
     (Socket => Client,
      Server =>
        (Family => Family_Inet, Addr => Inet_Addr ("127.0.0.1"),
         Port   => 12_321));

   Channel := Stream (Client);

   Read_Received.Start;

   Send_Msg
     (Channel,
      "PubKey:" & Trim (RSA.Public_Key_E'Image) & "," &
      Trim (RSA.Public_Key_N'Image));

   loop
      declare
         Message : constant String := Get_Line;
      begin
         Send_Msg (Channel, Message, True);
         Put_Line ("<: " & Message);
         exit when Message = "quit";
      end;
   end loop;

   Close_Socket (Client);
end Client_Main;
