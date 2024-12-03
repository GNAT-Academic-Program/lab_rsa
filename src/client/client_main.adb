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

   --This helper function that converts the hash to a hexadecimal string is courtesy of ChatGPT.
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
               --This obtains the hash certifiate from the received ciphertext.
               Hash_String: String := Extract_Hash;
               --This converts Hash_String to a digest format for verification.
               Hash_String_Digest: SHA2.SHA_256.Digest with Address => Hash_String'Address;
               --This extracts the encrypted message fro the erceived ciphertext.
               Encrypted_Msg: String := Extract_Encrypted_Msg;
               --This hashes the encrypted message with the SHA_256 algorithm.
               Encrypted_Hash: SHA2.SHA_256.Digest := Hash_Msg(Encrypted_Msg);
               --This converts received hash to hexadecimal.
               Hex_Hash_New: Unbounded_String := To_Unbounded_String(To_Hexadecimal(Encrypted_Hash));
               --To officially verify the hash, we convert the recived hash that is stored in the unbounded string to a regular string. The regular string is compared against hash recieved.
               Final_Hex_Hash: String(1 .. Length(Hex_Hash_New)) := To_String(Hex_Hash_New);
            begin
               if(Hash_String = Final_Hex_Hash) then
                  --Is_Same_Hash := True;
               else
                  --Is_Same_Hash := False;
               end if;
      

               if Is_Same_Hash then
                 
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
            --This extracts the encrypted mesage from what was received from the server.
            Encrypted_Msg: Unbounded_String := To_Unbounded_String(Encrypt_Msg (Msg, Partner_Pub_Key_E, Partner_Pub_Key_N));
            --This converts the extracted encrypted message to a string.
            Final_Encrypted_Msg: String(1 .. Length(Encrypted_Msg)) := To_String(Encrypted_Msg);
            --Using the SHA2 library, we hash the encrypted message with the SHA_256 algorithm.
            Message_Hash: SHA2.SHA_256.Digest := Hash_Msg(Final_Encrypted_Msg);
            --This converts the hashed message (the digest certificate) to hexadecimal and is stored in an unbounded string.
            Hash_String: Unbounded_String := To_Unbounded_String(To_Hexadecimal(Message_Hash));
            --This converts the unbounded string containing the hexadecimal digets certificate to a regular string.
            Final_Hash_String: String(1.. Length(Hash_String)) := To_String(Hash_String);
          

         begin
         --Hint: We need to send the encrypted message, hash and the terminator here.
            String'Write(Ch,"Msg:" & Final_Hash_String & Final_Encrypted_Msg & Terminator);
            exception 
               when Error: others =>
               Put_Line (Exception_Information (Error));

         end;
      
         
        
      else
         Put_Line(Msg'Img);
         --Hint: We need to send the key (essentialy an unencrypted message) along with the terminator here.
         --String'Write (Ch, Msg & Terminator);
         
      end if;
   end Send_Msg;

begin
   --Hint: We need to create a socket of type Client here.
   --Create_Socket (Socket => Client);
   --Hint: Please connect the Client socket to the server address of 127.0.0.1 and port 12_321.
   Connect_Socket
     (Socket => Client,
      Server =>
        (Family => Family_Inet, Addr => Inet_Addr (--"127.0.0.1"),
         Port   => --12_321));
   --Hint: We need to give stream access to the Client socket here.
   --Channel := Stream (Client);

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
   --Hint: We need to close the socket here.
   --Close_Socket (Client);
end Client_Main;