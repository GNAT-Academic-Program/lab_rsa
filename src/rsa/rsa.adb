with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Numerics.Big_Numbers; use Ada.Numerics.Big_Numbers;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics; use Ada.Numerics;
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

-- helper function Mod_Pow and Is_Prime with Miller-Rabin test function are courtesy of Cursor AI.
   function Mod_Pow (Base, Exp, M : Big_Integer) return Big_Integer is
      Result : Big_Integer := 1;
      B : Big_Integer := Base mod M;
      E : Big_Integer := Exp;
   begin
      while E > 0 loop
         if E mod 2 = 1 then
            Result := (Result * B) mod M;
         end if;
         B := (B * B) mod M;
         E := E / 2;
      end loop;
      return Result;
   end Mod_Pow;

   function Is_Prime (N : Big_Integer) return Boolean is
      function Miller_Rabin_Test (A : Big_Integer) return Boolean is
         D : Big_Integer := N - 1;
         X : Big_Integer;
         Y : Big_Integer;
         J : Natural := 0;
      begin
         -- Find largest power of 2 that divides N-1
         while D mod 2 = 0 loop
            D := D / 2;
            J := J + 1;
         end loop;

         -- Initial test
         X := Mod_Pow(A, D, N);
         if X = 1 or X = N - 1 then
            return True;
         end if;

         -- Square 'r-1' times
         for I in 1 .. J - 1 loop
            X := (X * X) mod N;
            if X = N - 1 then
               return True;
            end if;
            if X = 1 then
               return False;
            end if;
         end loop;

         return False;
      end Miller_Rabin_Test;

   begin
      -- Handle small numbers quickly
      if N <= 1 then
         return False;
      elsif N <= 3 then
         return True;
      elsif N mod 2 = 0 then
         return False;
      end if;

      -- Miller-Rabin test with first few prime numbers as bases
      -- These specific values provide strong guarantees for large numbers
      return Miller_Rabin_Test(To_Big_Integer(2)) and then
             Miller_Rabin_Test(To_Big_Integer(3)) and then
             Miller_Rabin_Test(To_Big_Integer(5)) and then
             Miller_Rabin_Test(To_Big_Integer(7)) and then
             Miller_Rabin_Test(To_Big_Integer(11));
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
      while not isPrime or not isPositive loop
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

   function Compute_N (P, Q :in out Big_Integer) return Big_Integer is
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
         Coprime_Result : Big_Integer := 0;
         Final_Result: Big_Integer := 0;
      begin
         Coprime_Result :=
              Greatest_Common_Divisor
                (To_Big_Integer (3), Phi);
         if(Coprime_Result /=1) then
            Final_Result := To_Big_Integer(65537);
         else
            Final_Result := To_Big_Integer(3);

         end if;
      --end;
         return Final_Result;
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
   --To_Str and To_Int are courtesy of ChatGPT
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
   
   --Hash_Msg lines 318 to 334 for cleaning the hash is courtesy of ChatGPT
   function Hash_Msg(StringVal: String) return SHA2.SHA_256.Digest is
      Original_Hash : SHA2.SHA_256.Digest;
      Cleaned_Bytes : SHA2.SHA_256.Digest := (others => 0); -- Placeholder for the cleaned digest
      Cleaned_Length : Ada.Streams.Stream_Element_Offset := 0;  -- Tracks the length of the cleaned digest
   begin
      -- Compute the hash
      Original_Hash := SHA2.SHA_256.Hash2(StringVal);

      -- Iterate through the original hash bytes
      for Index in Original_Hash'Range loop
         Put_Line("string of hash index" & Original_Hash(Index)'Image);
         -- Filter out unwanted characters (e.g., newlines, spaces, null bytes)
         if Original_Hash(Index) /= Character'Pos(' ') and then
            Original_Hash(Index) /= Character'Pos(Character'Val(10)) and then
            Original_Hash(Index) /= Character'Pos(Character'Val(13)) and then
            Original_Hash(Index) /= 0 then
            
            -- Ensure Cleaned_Length doesn't exceed the bounds
            if Cleaned_Length < Cleaned_Bytes'Last then
               Cleaned_Length := Cleaned_Length + 1;
               Cleaned_Bytes(Cleaned_Length) := Original_Hash(Index);
            else
               -- Handle the case where the cleaned digest exceeds bounds
               exit; -- Or raise an error, depending on your application logic
            end if;
         end if;
      end loop;

  
      return Cleaned_Bytes;
   end Hash_Msg;



   function Encrypt_Msg
     (Msg : String; Pub_Key_E, Pub_Key_N : Big_Integer) return String
   is
      Nbr_Bytes_Per_Chunk : constant Integer := 1; --this is the max number of bytes

      function Sanitize_Msg (M : String) return String is
         --What should To_Pad contain ? 
         --Hint: the amount of bytes needed be 'filler' are calculated by subtracting (the length of the message mod the total number of bytes per chunk) from the total number of bytes per chunk.
         To_Pad : constant Integer :=
           --Nbr_Bytes_Per_Chunk - (M'Length mod (Nbr_Bytes_Per_Chunk));
           --What should San_Msg contain ?
           --Hint: the fully santized message is made of the message sent and any required fillers.
         San_Msg : constant String := -- M & To_Pad * Filling;
      begin
      
         return San_Msg;
      end Sanitize_Msg;

      

      function Number_Of_Words (M : String) return Integer is
        -- Hint: here we calculate the number of chunks in the plaintext.
        --(M'Length / Nbr_Bytes_Per_Chunk);

      Sanitized_Msg : constant String        := Sanitize_Msg (Msg);
      Nbr_Words     : constant Integer := Number_Of_Words (Sanitized_Msg);
      W             : --Words (1 .. Nbr_Words) := [others => 0];

      function Build_Encrypted_Msg
        (W : Words; Idx : Integer := 1) return String
      is
      begin
    
         if Idx < W'Last then
            return
              -- Trim (W (Idx)'Image) & "," & Build_Encrypted_Msg (W, Idx + 1);
         else
            --return Trim (W (Idx)'Image) & ",";
         end if;
      end Build_Encrypted_Msg;

   begin
      for I in W'Range loop
         declare
            Idx : constant Integer := ((I - 1) * Nbr_Bytes_Per_Chunk) + 1;
         begin
            
            --Hint:to split the plaintext into chunks of bytes, we do: Sanitized_Msg (Idx .. Idx + Nbr_Bytes_Per_Chunk - 1)
            --Hint: This is where the sanitized chunk is converted to an inetger and is filled into the W array.
            W (I) :=
              --To_Int (Sanitized_Msg (Idx .. Idx + Nbr_Bytes_Per_Chunk - 1));

            Put_Line(" word bit as int; " & W(I)'Image);
         end;
      end loop;

      for I in W'Range loop
         --Hint: We need to encrypt each bit of the plaintext which is now in integer form stored in the W array.
         W (I) := -- Encrypt (W (I), Pub_Key_E, Pub_Key_N);
 
      end loop;

      declare
         Encrypted_Msg : constant String := Build_Encrypted_Msg (W);
      begin
         Put_Line("official encyrpted message:"& Encrypted_Msg);
         return Encrypted_Msg;
      exception
         when others =>
            Put_Line("Error in Build_Encrypted_Msg");
            raise;
        
      end;
   end Encrypt_Msg;

   function Find_Next_Word (Msg : String; S : Integer; E : in out Integer)
      return String
   is
   begin
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
      S : --Integer := Msg'First;
      E : --Integer := Msg'Last;
      temp: Integer;
      function Number_Of_Words return Integer is
      --Hint: this is the counter to track the number of chunks that the ciphertext has.
         Comma_Count : Integer := 0;
      begin
         for I in Msg'Range loop
        
            if Msg (I) = ',' then
               -- Comma_Count := Comma_Count + 1;
            end if;
         end loop;

         Put_Line("Total commas (word count): " & Comma_Count'Image);
         return Comma_Count;     
      end Number_Of_Words;

      W : Words (1 .. Number_Of_Words) := [others => 0];

      function Build_Decrypted_Msg
        (W : Words; Idx : Integer := 1) return String is
        
      begin
        
         if Idx < W'Last then
              return --To_Str (Decrypt (W (Idx))) & Build_Decrypted_Msg (W, Idx + 1);
         else
            return --To_Str (Decrypt (W (Idx)));
         end if;
      
      end Build_Decrypted_Msg;
   begin
      -- Hint: This is where we build the array W of encrypted chunks.
      for I in W'Range loop
         W(I) := --From_String(Find_Next_Word (Msg, S, E));
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