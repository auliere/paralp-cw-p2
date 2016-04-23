---------------------------------------------------------
------------------PARALLEL PROGRAMMING-------------------
-------------------COURSE WORK PART 3--------------------
----               --ADA. RENDEZVOUZ--              -----
----              MA = MB*MC + MO*ME*a              -----
---------------------------------------------------------
-------------     BODY OF DATA MODULE     ---------------
---------------------------------------------------------
-----------------CREATED ON 20.03.2016-------------------
----------------BY OLEG PEDORENKO, IP-31-----------------
---------------------------------------------------------

with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;
with Ada.Containers.Generic_Constrained_Array_Sort;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Discrete_Random;

package body Data is

  maxOutputSize: Integer := 8;
      
  procedure Get_Random(A: out Scalar) is
    use Ada.Numerics.Float_Random;
    subtype R is Integer range 1 .. Random_Max;
    package Rand_Int is new Ada.Numerics.Discrete_Random (R);
    G1: Generator;
    G2: Rand_Int.Generator;
  begin
    Reset(G1); Rand_Int.Reset(G2);
    A := Random(G1) * Scalar(Rand_Int.Random(G2));
  end Get_Random;
  
  function Cmp(Left: Scalar; Right: Scalar) return Boolean;
  procedure Desc_Sort is 
    new Ada.Containers.
      Generic_Constrained_Array_Sort(Range_T, Scalar, Vector, Cmp);
  
  procedure Vector_Input(A: out Vector);
  procedure Vector_Output(A: in Vector);  
  procedure Proto_Matrix_Input(A: out Proto_Matrix);
  procedure Proto_Matrix_Output(A: in Proto_Matrix);

  procedure Vector_Input(A: out Vector) is
  begin
    if Size <= maxOutputSize then
      for I in A'Range loop
        Input(A(I));
      end loop;
    else
      for I in A'Range loop
        Input(A(I));
      end loop;    
    end if;
  end Vector_Input;

  procedure Vector_Output(A: in Vector) is
  begin
    for I in A'Range loop
      Output(A(I));
    end loop;
    New_Line;
  end Vector_Output;

  procedure Proto_Matrix_Input(A: out Proto_Matrix) is
  begin
    for I in A'Range loop
      Vector_Input(A(I));
    end loop;
  end Proto_Matrix_Input;
  
  procedure Proto_Matrix_Output(A: in Proto_Matrix) is
  begin
    for I in A'Range loop
      Vector_Output(A(I));
    end loop;  
  end Proto_Matrix_Output;  
  
  procedure Input(A: out Vector) is 
  begin 
    Vector_Input(A);
    if Size <= maxOutputSize then 
      Vector_Output(A);
    end if;
  end Input;
  
  procedure Input(A: out Proto_Matrix) is
  begin 
    Proto_Matrix_Input(A);
    if Size <= maxOutputSize then
      Proto_Matrix_Output(A);
    end if;
  end Input;
  
  procedure Input(A: out Scalar) is
  begin
    --Get(A);
    --Get_Random(A);
    A := 1.0;
  end Input;
  
  procedure Output(A: in Vector) is 
  begin
    if Size <= maxOutputSize then 
      Vector_Output(A);
    end if;
  end Output;
   
  procedure Output(A: in Proto_Matrix) is 
  begin
    if Size <= maxOutputSize then
      Proto_Matrix_Output(A);
    end if;
  end Output;
  
  procedure Output(A: in Scalar) is 
  begin
    Put(A, 7, 2, 0);
  end Output;
  
  procedure Fill(A: out Vector) is
  begin
    for I in A'Range loop
      A(I) := 0.0;
    end loop;
  end Fill;
  
  procedure Fill(A: out Proto_Matrix) is
  begin
    for I in A'Range loop
      Fill(A(I));
    end loop;
  end Fill;

  function Cmp(Left: Scalar; Right: Scalar) return Boolean is
  begin
    return (Left > Right);
  end Cmp;
  
end Data;