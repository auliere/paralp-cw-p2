generic 
	Size: Integer;
	Random_Max: Integer;
package Data is
	subtype Range_T is Integer range 1 .. Size;
  subtype Scalar is Float;
	type Vector is array(Range_T) of Scalar;
	type Matrix is array(Range_T) of Vector;
	
  procedure Fill(A: out Vector);
  procedure Fill(A: out Matrix);
  
	procedure Input(A: out Vector);
	procedure Input(A: out Matrix);
  procedure Input(A: out Scalar);
	
	procedure Output(A: in Vector);
	procedure Output(A: in Matrix);
	procedure Output(A: in Scalar);
	
private


end Data;