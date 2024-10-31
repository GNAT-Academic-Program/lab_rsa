with Ada.Text_IO; use Ada.Text_IO;
--with SHA2_Generic_32;
with Ada.Streams; use Ada.Streams; -- Add this
with Interfaces; use Interfaces; -- Ensure this is included
--with SHA2_Generic; 
with SHA2; use SHA2;
with Ada.Text_IO; use Ada.Text_IO;
package RSA is
   function Encrypt
     (Data : Integer; Pub_Key_E, Pub_Key_N : Integer) return Integer;
   function Decrypt (Cypher : Integer) return Integer;

   function Encrypt_Msg
     (Msg : String; Pub_Key_E, Pub_Key_N : Integer) return String;
   function Decrypt_Msg (Msg : String) return String;
   function Hash_Msg(StringVal : String) return SHA2.SHA_256.Digest;

   function Public_Key_N return Integer;
   function Public_Key_E return Integer;
end RSA;
