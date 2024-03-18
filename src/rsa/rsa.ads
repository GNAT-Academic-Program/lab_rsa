package RSA is
   function Encrypt
     (Data : Integer; Pub_Key_E, Pub_Key_N : Integer) return Integer;
   function Decrypt (Cypher : Integer) return Integer;

   function Encrypt_Msg
     (Msg : String; Pub_Key_E, Pub_Key_N : Integer) return String;
   function Decrypt_Msg (Msg : String) return String;

   function Public_Key_N return Integer;
   function Public_Key_E return Integer;
end RSA;
