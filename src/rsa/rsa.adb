with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;

with Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Numerics.Big_Numbers.Big_Integers;

with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Utils; use Utils;

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

   subtype Prime_Number is Positive range 2 .. Positive'Last with
       Dynamic_Predicate =>
        (for all I in 2 .. (Prime_Number / 2) => (Prime_Number mod I) /= 0);

   subtype Prime_Range is Positive range 1 .. 1_000;
   type Prime_Array is array (Prime_Range) of Prime_Number;

   package Rand_Idx is new Ada.Numerics.Discrete_Random (Prime_Range);

   Primes : Prime_Array;

   procedure Generate_Primes is
      Idx        : Integer := 1;
      Test_Prime : Integer := 0;
   begin
      while Idx <= Prime_Range'Last loop
         if Test_Prime in Prime_Number then
            Primes (Idx) := Test_Prime;
            Idx          := Idx + 1;
         end if;
         Test_Prime := Test_Prime + 1;
      end loop;
   end Generate_Primes;

   function Extended_GCD
     (A, B : Big_Integer; X, Y : out Big_Integer) return Big_Integer
   is
   begin
      if B = 0 then
       --  X := 1;
       --  Y := 0;
        -- return A;
      else
         declare
            X1, Y1 : Big_Integer;
           -- GCD    : constant Big_Integer := Extended_GCD (B, A mod B, X1, Y1);
         begin
            X := Y1;
          --  Y := X1 - (A / B) * Y1;
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
      return To_Big_Integer (Primes (Rand_Idx.Random (Gen)));
   end Pick_P;

   function Pick_Q (P : Big_Integer) return Big_Integer is
      Gen : Rand_Idx.Generator;
      Q   : Big_Integer := P;
   begin
      Rand_Idx.Reset (Gen);
      while P = Q loop
         Q := To_Big_Integer (Primes (Rand_Idx.Random (Gen)));
      end loop;
      return Q;
   end Pick_Q;

   function Compute_N (P, Q : Big_Integer) return Big_Integer is
   begin
      return P * Q;
   end Compute_N;

   function Compute_Phi (P, Q : Big_Integer) return Big_Integer is
   begin
      return (P - 1) * (Q - 1);
   end Compute_Phi;

   function Select_E (Phi : Big_Integer) return Big_Integer is
      End_Idx : Prime_Range;
   begin
      for I in Prime_Range'Range loop
         if To_Big_Integer (Primes (I)) < Phi then
            End_Idx := I;
         end if;
      end loop;
      declare
         subtype Prime_Range_E is Prime_Range range 1 .. End_Idx;
         package Rand_Coprime is new Ada.Numerics.Discrete_Random
           (Prime_Range_E);
         Gen_Coprime    : Rand_Coprime.Generator;
         Idx_Rand       : Prime_Range;
         Coprime_Result : Big_Integer := 0;
      begin
         while Coprime_Result /= 1 loop
            Rand_Coprime.Reset (Gen_Coprime);
            Idx_Rand       := Rand_Coprime.Random (Gen_Coprime);
            Coprime_Result :=
              Greatest_Common_Divisor
                (To_Big_Integer (Primes (Idx_Rand)), Phi);
         end loop;

         return To_Big_Integer (Primes (Idx_Rand));
      end;
   end Select_E;

   procedure Generate_Keys is
      P, Q, N, Phi, E, D : Big_Integer := 0;
   begin
      while D = 0 loop
       --  P   := Pick_P;
       --  Q   := Pick_Q (P);
       --  N   := Compute_N (P, Q);
       --  Phi := Compute_Phi (P, Q);
       --  D   := Mod_Inverse (E, Phi);
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
     (Data : Integer; Pub_Key_E, Pub_Key_N : Integer) return Integer
   is
   begin
      return
        To_Integer
          (Power_Mod)
           --  (To_Big_Integer (Data), To_Big_Integer (Pub_Key_E),
             -- To_Big_Integer (Pub_Key_N)));
   end Encrypt;

   function Decrypt (Cypher : Integer) return Integer is
   begin
      return
        To_Integer
          (Power_Mod (--To_Big_Integer (Cypher), Priv_Key.D, Priv_Key.N));
           )) end Decrypt;

   function To_Str (I : Integer) return String is
      S : String (1 .. 4);
      for S'Address use I'Address;
   begin
      return S;
   end To_Str;

   function To_Int (S : String) return Integer is
      Base : String (1 .. 4) := [others => ASCII.NUL];
      D    : Integer         := 0;
      for D'Address use Base'Address;
   begin
      Base (1 .. S'Length) := S;
      return D;
   end To_Int;

   Filling : constant String := "*";
   type Words is array (Positive range <>) of Integer;

   function Encrypt_Msg
     (Msg : String; Pub_Key_E, Pub_Key_N : Integer) return String
   is
      Nbr_Bytes_Per_Chunk : constant Integer := 2;

      function Sanitize_Msg (M : String) return String is
         To_Pad : constant Integer :=
           Nbr_Bytes_Per_Chunk - (M'Length mod (Nbr_Bytes_Per_Chunk));
         San_Msg : constant String := M & To_Pad * Filling;
      begin
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
            W (I) :=
              To_Int (Sanitized_Msg (Idx .. Idx + Nbr_Bytes_Per_Chunk - 1));
         end;
      end loop;

      for I in W'Range loop
         W (I) := Encrypt (W (I), Pub_Key_E, Pub_Key_N);
      end loop;

      declare
         Encrypted_Msg : constant String := Build_Encrypted_Msg (W);
      begin
         return Encrypted_Msg;
      end;
   end Encrypt_Msg;

   function Find_Next_Word (Msg : String; S : Integer; E : in out Integer)
      return String
   is
   begin
      E := Msg'Last;
      for I in S .. E loop
         if Msg (I) = ',' then
            E := I - 1;
            return Msg (S .. E);
         end if;
      end loop;
      return Msg (S .. Msg'Last);
   end Find_Next_Word;

   function Decrypt_Msg (Msg : String) return String is
      S : Integer := Msg'First + 1;
      E : Integer := Msg'Last;

      function Number_Of_Words return Integer is
         Comma_Count : Integer := 0;
      begin
         for I in Msg'Range loop
            if Msg (I) = ',' then
               Comma_Count := Comma_Count + 1;
            end if;
         end loop;
         return Comma_Count;
      end Number_Of_Words;

      W : Words (1 .. Number_Of_Words) := [others => 0];

      function Build_Decrypted_Msg
        (W : Words; Idx : Integer := 1) return String
      is
      begin
         if Idx < W'Last then
            return
              To_Str (Decrypt (W (Idx))) & Build_Decrypted_Msg (W, Idx + 1);
         else
            return To_Str (Decrypt (W (Idx)));
         end if;
      end Build_Decrypted_Msg;
   begin
      for I in W'Range loop
         W (I) := Integer'Value (Find_Next_Word (Msg, S, E));
         S     := E + 2;
      end loop;
      return Build_Decrypted_Msg (W);
   end Decrypt_Msg;

   function Public_Key_N return Integer is
   begin
      -- return To_Integer (Pub_Key.N);
   end Public_Key_N;

   function Public_Key_E return Integer is
   begin
      -- return To_Integer (Pub_Key.E);
   end Public_Key_E;

begin
   Generate_Primes;
   Generate_Keys;
   Put_Line ("Public Key: " & Pub_Key'Image);
   Put_Line ("Private Key: " & Priv_Key'Image);
end RSA;
