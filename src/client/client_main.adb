with GNAT.Sockets; use GNAT.Sockets;
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Streams;  use Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Exceptions;  use Ada.Exceptions;

with Ada.Streams; use Ada.Streams; -- Add this
with Interfaces; use Interfaces; -- Ensure this is included

with SHA2; use SHA2;
with Ada.Text_IO; use Ada.Text_IO;
with RSA;   use RSA;
with Utils; use Utils;

procedure Client_Main is
   Client  : Socket_Type;
   Channel : Stream_Access;

   Partner_Pub_Key_E : Big_Integer := 0;
   Partner_Pub_Key_N : Big_Integer := 0;

   Terminator : constant Character := ASCII.NUL;

   Initial_Hash : SHA2.SHA_256.Digest;
   Final_Hash : SHA2.SHA_256.Digest;
   Is_Same_Hash: Boolean;

   task Read_Received is
      entry Start;
   end Read_Received;

   function To_Hexadecimal (Data : SHA2.SHA_256.Digest) return String is
         Hex : constant array(0 .. 15) of Character := "0123456789ABCDEF";
         Result : String(1 .. Data'Length * 2); -- Each byte becomes 2 characters in hex
         Index : Integer := 0;
      begin
         for Byte of Data loop
            Index := Index + 1;
            -- High nibble (the first half of the byte)
            Result(Index * 2 - 1) := Hex(Integer(Byte) / 16);
            -- Low nibble (the second half of the byte)
            Result(Index * 2) := Hex(Integer(Byte) mod 16);
         end loop;
         return Result;
      end To_Hexadecimal;

   task body Read_Received is
      

      function Get_Message return String is
         Offset : Stream_Element_Count;
         Data   : Stream_Element_Array (1 .. 1);
         C      : Character;
         Ready : Boolean;
      begin
         Read(Channel.all, Data, Offset);
         C := Character'Val (Data (Data'First));
            if C = Terminator then
               return "";
            else
               return C & Get_Message;
            end if;
      end Get_Message;

      function Filter_Message return String is
         Msg : constant String := Get_Message;

         procedure Extract_Key (M :String) is
            Ms : String := M(M'First .. M'Last);
         begin
            
            for C in Ms'Range loop
               if Ms (C) = ',' then
                  Partner_Pub_Key_E := From_String (Ms (8 .. C - 1));
                  Partner_Pub_Key_N := From_String (Ms (C + 1 .. Ms'Last));
                  exit;
               end if;
            end loop;
         exception
            when Error: others =>
               Put_Line (Exception_Information (Error));
         end Extract_Key;

         function Extract_Hash return String is
            (Msg(5 .. 68));
         function Extract_Encrypted_Msg return String is
            (Msg(69 .. Msg'Last));
      begin
         Put_Line("Message: " & Msg);
         Put_Line("Length: " & Msg'Length'Image);
         Put_Line("PubKey: " & "PubKey:" & Msg(1 .. 7));

         Put_Line(Msg'Length'Image);
         Put_Line(Msg(1 .. 4));

         if Msg'Length > 7 and then Msg (1 .. 7) = "PubKey:" then
            Extract_Key (Msg);
            Put_Line
              (":> Received Partner Public Key: " & "(" &
               Partner_Pub_Key_E'Image & "," & Partner_Pub_Key_N'Image & ")");
         elsif Msg'Length > 4 and then Msg(1 .. 4) = "Msg:" then   
            declare
               Hash_String: String := Extract_Hash;
               Hash_String_Digest: SHA2.SHA_256.Digest with Address => Hash_String'Address;
               Encrypted_Msg: String := Extract_Encrypted_Msg;
               Encrypted_Hash: SHA2.SHA_256.Digest := Hash_Msg(Encrypted_Msg);
               Hex_Hash_New: Unbounded_String := To_Unbounded_String(To_Hexadecimal(Encrypted_Hash));
               Final_Hex_Hash: String(1 .. Length(Hex_Hash_New)) := To_String(Hex_Hash_New);
            begin
               Put_Line("old hash:" & Hash_String);
               Put_Line("new hash:" & Final_Hex_Hash);
               Put_Line("encrypted message:"& Encrypted_Msg);
               if(Hash_String = Final_Hex_Hash) then
                  Is_Same_Hash := True;
               else
                  Is_Same_Hash := False;
               end if;
               --  for I in Encrypted_Hash'range loop
               --     Put_Line("Initial_Hash: " & Hash_String_Digest(I)'Image & " and Final Hash: " & Encrypted_Hash(I)'Image);
               --     if(Hash_String_Digest(I)) = Encrypted_Hash(I) then
               --        Is_Same_Hash := True;
               --     else
               --        Is_Same_Hash := False;
               --        exit;
               --     end if;
               --  end loop;

               Put_Line(Is_Same_Hash'Image);

               if Is_Same_Hash then
                  Put_Line("sdjkf " & Encrypted_Msg);
                  Put_Line (":> Decrypted: " & Decrypt_Msg (Encrypted_Msg));
                  Ada.Text_IO.Flush;
               

               else
                  Put_Line("invalid");
               end if;

            end;
         else
            Put_Line ("we are in the else");
   
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
            Put_Line("ghghjgjhjj");
            Put_Line(Msg'Img);
            exit when Msg = "quit";
         end;
      end loop;
   end Read_Received;

   procedure Send_Msg
     (Ch : Stream_Access; Msg : String; Encrypted : Boolean := False)
   is
   begin
      if Encrypted then
         declare
            Encrypted_Msg: Unbounded_String := To_Unbounded_String(Encrypt_Msg (Msg, Partner_Pub_Key_E, Partner_Pub_Key_N));
            Final_Encrypted_Msg: String(1 .. Length(Encrypted_Msg)) := To_String(Encrypted_Msg);
            Message_Hash: SHA2.SHA_256.Digest := Hash_Msg(Final_Encrypted_Msg);
            Hash_String: Unbounded_String := To_Unbounded_String(To_Hexadecimal(Message_Hash));
            Final_Hash_String: String(1.. Length(Hash_String)) := To_String(Hash_String);
          
            --Hash_String: String(1 .. Messeage_Hash'Length) with Address => Message_Hash'Address;

         begin
          
            Put_Line(Final_Hash_String);
            Put_Line("separator");
            Put_Line(Final_Encrypted_Msg);
            String'Write(Ch,"Msg:" & Final_Hash_String & Final_Encrypted_Msg & Terminator);
            exception 
               when Error: others =>
               Put_Line (Exception_Information (Error));

     
            
         end;
      
         
        
      else
         Put_Line("sendin out key");
         Put_Line(Msg'Img);
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
   begin
      Send_Msg
      (Channel,
         "PubKey:" & Trim (RSA.Public_Key_E'Image) & "," &
         Trim (RSA.Public_Key_N'Image));
   exception
      when others => 
         Put_Line("failed to send message");
   end;

   Put_Line("in client");

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