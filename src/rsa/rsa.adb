with Ada.Text_IO; use Ada.Text_IO;

with Ada.Numerics.Big_Numbers; use Ada.Numerics.Big_Numbers;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Utils; use Utils;
with Ada.Streams; use Ada.Streams; -- Add this
with Interfaces; use Interfaces; -- Ensure this is included
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces; -- Ensure this is included
with SHA2; use SHA2;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics; use Ada.Numerics;

with Ada.Numerics.Elementary_Functions;
use  Ada.Numerics.Elementary_Functions;

package body RSA is

   type Key is (Pub, Priv);

   type Key_Data (Kind : Key := Pub) is record
      N : Big_Integer;
      case Kind is
         when Pub =>
            E : Big_Integer;
         when Priv =>
            D : Big_Integer;
      end case; 
   end record;

   Pub_Key  : Key_Data (Pub);
   Priv_Key : Key_Data (Priv);

   function Is_Prime (N : Big_Integer) return Boolean is
      I : Big_Integer := 3;
   begin
      if N < 2 then
         return False;
      elsif N = 2 then
         return True; -- 2 is prime
      elsif N mod 2 = 0 then
         return False; -- Exclude even numbers
      end if;

     
      while I * I <= N loop
         if N mod I = 0 then
            return False;
         end if;
         I := I + 2; -- Move to the next odd number
      end loop;

      return True; -- N is prime
   end Is_Prime;

   subtype Prime_Number is Big_Integer;


   subtype Prime_Range is Positive range 1 .. 10_000;  -- Define the range for primes
   package Rand_Idx is new Ada.Numerics.Discrete_Random(Prime_Range);
  
   function Get_Random_Prime return Big_Integer is
      tempInt : Integer;
      Random_Index : Big_Integer;
      isPrime: Boolean := False;
      isPositive: Boolean := False;
      package Rand_Idx is new Ada.Numerics.Discrete_Random (Integer);
      Gen : Rand_Idx.Generator;
   begin
      -- Generate a random index in the range of found primes
      Rand_Idx.Reset(Gen);
      while not isPrime and not isPositive loop
         tempInt := Rand_Idx.Random(Gen);
         Random_Index := To_Big_Integer(tempInt);
         Put_Line(Random_Index'Image);

         isPrime := Is_Prime(Random_Index);
         if Random_Index > 0 then
            isPositive := True;
         end if; 
      end loop;
      Put_Line("im done");
      return Random_Index;

   end Get_Random_Prime;


   function Extended_GCD
     (A, B : Big_Integer; X, Y : out Big_Integer) return Big_Integer
   is
   begin
      if B = 0 then
         X := 1;
         Y := 0;
         return A;
      else
         declare
            X1, Y1 : Big_Integer;
            GCD    : constant Big_Integer := Extended_GCD (B, A mod B, X1, Y1);
         begin
            X := Y1;
            Y := X1 - (A / B) * Y1;
            return GCD;
         end;
      end if;
   end Extended_GCD;

   function Mod_Inverse (E, Phi : Big_Integer) return Big_Integer is
      X, Y : Big_Integer;
      GCD  : constant Big_Integer := Extended_GCD (E, Phi, X, Y);
   begin
      if GCD = 1 then
         return (X mod Phi + Phi) mod Phi;
      else
         return 0;
      end if;
   end Mod_Inverse;

   function Pick_P return Big_Integer is
      Gen : Rand_Idx.Generator;
   begin
      Rand_Idx.Reset (Gen);
      Put_Line("p got");
      return Get_Random_Prime;
      
   end Pick_P;

   function Pick_Q (P : Big_Integer) return Big_Integer is
      Gen : Rand_Idx.Generator;
      Q   : Big_Integer := P;
   begin
      Rand_Idx.Reset (Gen);
      while P = Q loop
         Q := Get_Random_Prime;
      end loop;
      Put_Line("q got");
      return Q;
   end Pick_Q;

   function Compute_N (P, Q : Big_Integer) return Big_Integer is
   begin
      Put_Line("in computing N");
      return P * Q;
   end Compute_N;

   function Compute_Phi (P, Q : Big_Integer) return Big_Integer is
   begin
      Put_Line("ophi");
      return (P - 1) * (Q - 1);
   end Compute_Phi;

   function Select_E (Phi : Big_Integer) return Big_Integer is
      End_Idx : Big_Integer := 10;
      
   begin
      for I in 2 .. 10 loop
         if (Is_Prime(To_Big_Integer(I))) then
            if To_Big_Integer (I) < Phi then
               End_Idx := To_Big_Integer(I);
            end if;
         end if;
      end loop;
      declare
         subtype coprimeRange is Integer range 1 .. 20;
         package Rand_Coprime is new Ada.Numerics.Discrete_Random(coprimeRange);
         Gen_Coprime    : Rand_Coprime.Generator;
         Idx_Rand       : Prime_Range;
         Coprime_Result : Big_Integer := 0;
      begin
         
         while Coprime_Result /= 1 loop
            Rand_Coprime.Reset (Gen_Coprime);
            Idx_Rand       := Rand_Coprime.Random (Gen_Coprime);

            while not Is_Prime(To_Big_Integer(Idx_Rand)) loop
               Idx_Rand       := Rand_Coprime.Random (Gen_Coprime);
            end loop;

            Coprime_Result :=
               Greatest_Common_Divisor
                  (To_Big_Integer (Idx_Rand), Phi);
                  Put_Line("e calcs");
         end loop;

         return To_Big_Integer (Idx_Rand);
      end;
   end Select_E;

   procedure Generate_Keys is
      P, Q, N, Phi, E, D : Big_Integer := 0;
   begin
      while D = 0 loop
         P   := Pick_P;
         Q   := Pick_Q (P);
         N   := Compute_N (P, Q);
         Phi := Compute_Phi (P, Q);
         E   := Select_E (Phi);
         D   := Mod_Inverse (E, Phi);
      end loop;
      Pub_Key.N  := N;
      Pub_Key.E  := E;
      Priv_Key.N := N;
      Priv_Key.D := D;
   end Generate_Keys;

   function Power_Mod (M, D, N : Big_Integer) return Big_Integer is

      function Is_Odd (X : Big_Integer) return Boolean is (X mod 2 /= 0);

      Result : Big_Integer := 1;
      Exp    : Big_Integer := D;
      Mult   : Big_Integer := M mod N;
   begin
      while Exp /= 0 loop
         if Is_Odd (Exp) then
            Result := (Result * Mult) mod N;
         end if;

         Mult := Mult**2 mod N;
         Exp  := Exp / 2;
      end loop;

      return Result;
   end Power_Mod;

   function Encrypt
     (Data : Big_Integer; Pub_Key_E, Pub_Key_N : Big_Integer) return Big_Integer
   is
   begin
      return
        
          (Power_Mod
             ( (Data), Pub_Key_E, Pub_Key_N));
   end Encrypt;

   function Decrypt (Cypher : Big_Integer) return Big_Integer is
      
   begin
      return Power_Mod(Cypher, Priv_Key.D, Priv_Key.N);
   end Decrypt;

   function To_Str (I : Big_Integer) return String is
      Char_List : String := (1 .. 4 => ASCII.NUL);
      Local_I   : Big_Integer := I;
      Byte_Value : Integer;
      Index : Integer := 4;
   begin
   -- Extract each byte from the Big_Integer and convert it to a character
      while Local_I > 0 and Index > 0 loop
         Byte_Value := To_Integer(Local_I mod 256);
         if Byte_Value >= Character'Pos(Character'First) and Byte_Value <= Character'Pos(Character'Last) then
            Char_List(Index) := Character'Val(Byte_Value);
         else
            Char_List(Index) := ASCII.NUL;
         end if;
         Local_I := Local_I / 256;
         Index := Index - 1;
      end loop;
      return Char_List(Index + 1 .. 4);
   end To_Str;


   function To_Int (S : String) return Big_Integer is
      Result : Big_Integer := 0;
      Multiplier : Big_Integer := 1;
   begin
      for I in reverse S'Range loop
         Result := Result + (To_Big_Integer(Character'Pos(S(I))) * Multiplier);
         Multiplier := Multiplier * 256;
      end loop;
      return Result;
   end To_Int;

   
   Filling : constant String := "*";
   type Words is array (Positive range <>) of Big_Integer;
   
   function Hash_Msg(StringVal: String) return SHA2.SHA_256.Digest is
      type Char_Array is array (1 .. StringVal'Length) of Character;
      Result : Char_Array;
      Hash_Value : SHA2.SHA_256.Digest;  -- Ensure this matches the type defined in your SHA2_Generic_32


      begin
         Hash_Value := SHA2.SHA_256.Hash2(StringVal);
         Put_Line(Hash_Value'Image);
         return Hash_Value;
   end Hash_Msg;


   function Encrypt_Msg
     (Msg : String; Pub_Key_E, Pub_Key_N : Big_Integer) return String
   is
      Nbr_Bytes_Per_Chunk : constant Integer := 1; --this is the max number of bytes

      function Sanitize_Msg (M : String) return String is
         To_Pad : constant Integer :=
           Nbr_Bytes_Per_Chunk - (M'Length mod (Nbr_Bytes_Per_Chunk));
         San_Msg : constant String := M & To_Pad * Filling;
      begin
         Put_Line("sanitiszed message; " & San_Msg);
         return San_Msg;
      end Sanitize_Msg;

      

      function Number_Of_Words (M : String) return Integer is
        (M'Length / Nbr_Bytes_Per_Chunk);

      Sanitized_Msg : constant String        := Sanitize_Msg (Msg);
      Nbr_Words     : constant Integer := Number_Of_Words (Sanitized_Msg);
      W             : Words (1 .. Nbr_Words) := [others => 0];

      function Build_Encrypted_Msg
        (W : Words; Idx : Integer := 1) return String
      is
      begin
         Put_Line("num words; " & Nbr_Words'Image);
         if Idx < W'Last then
            return
              "," & Trim (W (Idx)'Image) & Build_Encrypted_Msg (W, Idx + 1);
         else
            return "," & Trim (W (Idx)'Image);
         end if;
      end Build_Encrypted_Msg;
   begin
      for I in W'Range loop
         declare
            Idx : constant Integer := ((I - 1) * Nbr_Bytes_Per_Chunk) + 1;
         begin
            Put_Line(" word bit convert; " & Sanitized_Msg (Idx .. Idx + Nbr_Bytes_Per_Chunk - 1));
           

            W (I) :=
              To_Int (Sanitized_Msg (Idx .. Idx + Nbr_Bytes_Per_Chunk - 1));

            Put_Line(" word bit as int; " & W(I)'Image);
         end;
      end loop;

      for I in W'Range loop
         
         W (I) := Encrypt (W (I), Pub_Key_E, Pub_Key_N);
         Put_Line("encrypted word bit: " & W(I)'Image);
      end loop;

      declare
         Encrypted_Msg : constant String := Build_Encrypted_Msg (W);
      begin
         Put_Line("official encyrpted messaGE: "& Encrypted_Msg);
         return Encrypted_Msg;
      end;
   end Encrypt_Msg;

   function Find_Next_Word (Msg : String; S : Integer; E : in out Integer)
      return String
   is
   begin
      Put_Line("Fidning next word");
      E := Msg'Last;
      Put_Line("this is E: " & E'Image);
      for I in S .. E loop
         if Msg (I) = ',' then
            E := I - 1;
            Put_Line("this is new E: " & E'Image);
            Put_Line("word to b returned: " & Msg(S .. E));
            return Msg (S .. E);
         end if;
      end loop;
      return Msg (S .. Msg'Last);
   end Find_Next_Word;


   function Decrypt_Msg (Msg : String) return String is
      type Big_Int_Vector is array (Positive range <>) of Big_Integer;
      S : Integer := Msg'First + 1;
      E : Integer := Msg'Last;
      temp: Integer;
      function Number_Of_Words return Integer is
         Comma_Count : Integer := 0;
      begin

         for I in Msg'Range loop
            Put_Line("index: " & I'Image);
            Put_Line(Msg(I)'Img);
            if Msg (I) = ',' then
               Comma_Count := Comma_Count + 1;
            end if;
         end loop;

         Put_Line("Total commas (word count): " & Comma_Count'Image);
         return Comma_Count;     
      end Number_Of_Words;

      W : Words (1 .. Number_Of_Words) := [others => 0];

      function Build_Decrypted_Msg
        (W : Words; Idx : Integer := 1) return String is
        
      begin
         Put_Line("decrypted ,message chunks: "  & (Decrypt (W (Idx))'Image));
         if Idx < W'Last then
              Put_Line("decrypted ,message chunks: "  & (Decrypt (W (Idx))'Image));
              return To_Str (Decrypt (W (Idx))) & Build_Decrypted_Msg (W, Idx + 1);
         else
            return To_Str (Decrypt (W (Idx)));
         end if;
      
      end Build_Decrypted_Msg;
   begin

      for I in W'Range loop
         Put_Line("I am in the loop");
         W(I) := To_Big_Integer(0);

         temp := Integer'Value(Find_Next_Word (Msg, S, E));
         W(I) := To_Big_Integer(temp);
         Put_Line("this is a word encrypted: " & W(I)'Image);
         S := E + 2;
      end loop;

      return Build_Decrypted_Msg (W);
   end Decrypt_Msg;

   function Public_Key_N return Big_Integer is
   begin
      return (Pub_Key.N);
   end Public_Key_N;

   function Public_Key_E return Big_Integer is
   begin
      return (Pub_Key.E);
   end Public_Key_E;

begin
   Generate_Keys;
   Put_Line ("Public Key: " & Pub_Key'Image);
   Put_Line ("Private Key: " & Priv_Key'Image);
end RSA;
