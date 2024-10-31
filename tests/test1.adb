with Ada.Text_IO; use Ada.Text_IO;
--with SHA2_Generic_32;
with Ada.Streams; use Ada.Streams; -- Add this
with Interfaces; use Interfaces; -- Ensure this is included
--with SHA2_Generic; 
with SHA2; use SHA2;
with Ada.Text_IO; use Ada.Text_IO;
procedure test1 is

    Dummy_Array : Stream_Element_Array(1 .. 8) := (1, 2, 3, 4, 5, 6, 7, 8);  -- Example values

    StringVal : String := "fart";
    type Char_Array is array (1 .. StringVal'Length) of Character;
    Result : Char_Array;
    

    Hash_Value : SHA2.SHA_256.Digest;  -- Ensure this matches the type defined in your SHA2_Generic_32
    Hash_Val2: SHA2.SHA_256.Digest;
    
begin
    Hash_Value := SHA2.SHA_256.Hash(Dummy_Array);

    Hash_Value := SHA2.SHA_256.Hash2("f");
    Hash_Val2 := SHA2.SHA_256.Hash2("f");

    Put_Line(Hash_Value'Image);
    Put_Line(Hash_Val2'Image);

    for I in Hash_Value'range loop 
        if(Hash_Value(I)) = Hash_Val2(I) then
            Put_Line("true");

        else
            Put_Line("false");
        end if;
    end loop;
  
end test1;