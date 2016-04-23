---------------------------------------------------------
------------------PARALLEL PROGRAMMING-------------------
-------------------COURSE WORK PART 2--------------------
----       --ADA. SEMAPHORES. PROTECTED UNITS--     -----
----              MA = MB*MC + MO*ME*a              -----
---------------------------------------------------------
--------     SPECIFICATION OF DATA MODULE     -----------
---------------------------------------------------------
-----------------CREATED ON 20.03.2016-------------------
----------------BY OLEG PEDORENKO, IP-31-----------------
---------------------------------------------------------

generic 
	Size: Integer;
	Random_Max: Integer;
package Data is
  subtype Range_T is Integer range 1 .. Size;
  
  subtype Scalar is Float;
  type Vector is array(Range_T) of Scalar;
  type Proto_Matrix is array(Integer range <>) of Vector;
  subtype Matrix is Proto_Matrix(Range_T);
  
  
	
  procedure Fill(A: out Vector);
  procedure Fill(A: out Proto_Matrix);
  
  procedure Input(A: out Vector);
  procedure Input(A: out Proto_Matrix);
  procedure Input(A: out Scalar);
	
  procedure Output(A: in Vector);
  procedure Output(A: in Proto_Matrix);
  procedure Output(A: in Scalar);
	
end Data;