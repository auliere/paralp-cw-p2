---------------------------------------------------------
------------------PARALLEL PROGRAMMING-------------------
-------------------COURSE WORK PART 2--------------------
----       --ADA. SEMAPHORES. PROTECTED UNITS--     -----
----              MA = MB*MC + MO*ME*a              -----
-----------------CREATED ON 20.03.2016-------------------
----------------BY OLEG PEDORENKO, IP-31-----------------
---------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Synchronous_Task_Control; use Ada.Synchronous_Task_Control;
with Ada.Float_Text_IO; 	use Ada.Float_Text_IO;
with Ada.Real_Time;			use Ada.Real_Time;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Data;


procedure P02 is
  N: Integer := 8;
  P: Integer := 2;
  H: Integer := 4;
  
begin
  --Read N and P from command line
	if(Argument_Count > 0) then 
		N := Integer'Value(Argument(1));
	end if;
  
  if(Argument_Count > 1) then 
		P := Integer'Value(Argument(2));
	end if;
  
  H := N/P;
  
  declare    
    -- Types
    package Data_S is new Data(N, 100); use Data_S;
    subtype Range_P is Integer range 1..(P-1);
    -- Variables
		MA, MB, MC, MO, ME: aliased Matrix;
		a: aliased Scalar;
    
    --SEMAPHORES
    Sem: array(Range_P) of Suspension_Object;
    
    protected Shared_Vars is
      procedure Write (MB, MO: in Matrix; a: in Scalar);
      entry Read (MB, MO: out Matrix; a: out Scalar);
    private
      MB_sh, MO_sh: Matrix;
      a_sh: Scalar;
      Read_Allowed: Boolean := False;
    end Shared_Vars;
    
    protected body Shared_Vars is
      procedure Write(MB, MO: in Matrix; a: in Scalar) is
      begin
        MB_sh := MB;
        MO_sh := MO;
        a_sh := a;
        Read_Allowed := True;
      end Write;
      
      entry Read(MB, MO: out Matrix; a: out Scalar) when Read_Allowed is
      begin
        MB := MB_sh;
        MO := MO_sh;
        a := a_sh;
      end Read;
    end Shared_Vars;
    
    procedure Calculate
      (MBa, MCa, MOa, MEa: access Matrix; aa: access Scalar; Q: Integer) is
      Sum1, Sum2: Scalar;
    begin
      for I in 1..N loop
        for J in (H*Q + 1)..(H*(Q+1)) loop
          Sum1 := 0.0;
          Sum2 := 0.0;
          for K in 1..N loop
            Sum1 := Sum1 + (MBa.all(I)(K) * MCa.all(K)(J)); --MB * MCh
            Sum2 := Sum2 + (MOa.all(I)(K) * MEa.all(K)(J)); -- MO * MEh
          end loop;
          MA(I)(J) := Sum1 + Sum2 * aa.all;
        end loop;
      end loop;
    end;
    
    -- TASK 1 --
    task T1 is
    	pragma Storage_Size(1000000000);
    end T1;
    
    task body T1 is
      MB1, MO1: aliased Matrix;
      a1: aliased Scalar;
    begin
      Put_Line("Task T 1 started");
      --Initialize MA
      Fill(MA);
      --Enter MB, MC, MO, ME, a
      Put_Line("MB =   "); Input(MB); 
      Put_Line("MC =   "); Input(MC); 
      Put_Line("MO =   "); Input(MO); 
      Put_Line("ME =   "); Input(ME);      
      Put_Line("a =    "); Input(a); Put(a); Put_Line("");
      --Signal SN,1
      Shared_Vars.Write(MB, MO, a);
      --Copy MB, MO, a
      Shared_Vars.Read(MB1, MO1, a1);
      --Calculate MAh := MB * MCh + MO * MEh * a
      Calculate(MB1'Access, MC'Access, MO1'Access, ME'Access, a1'Access, 0);
      --Wait WN,1
      for I in Sem'Range loop
        Suspend_Until_True(Sem(I));
      end loop;
      --Output MA
      Put_Line("MA =   "); Output(MA);
      Put_Line("Task T 1 finished");
    end T1;    
    
    -- OTHER TASKS --
    task type Generic_Task(I: Integer) is
      pragma Storage_Size(1000000000);
    end Generic_Task;

    task body Generic_Task is
      MB1, MO1: aliased Matrix;
      a1: aliased Scalar;      
    begin
      Put_Line("Task T" & Integer'Image(I+1) &" started");
      
      --Wait W1,1
      --Copy MB, MO, a
      Shared_Vars.Read(MB1, MO1, a1);
      --Calculate MAh
      Calculate(MB1'Access, MC'Access, MO1'Access, ME'Access, a1'Access, I);      
      --Signal S1,1
      Set_True(Sem(I));
      Put_Line("Task T" & Integer'Image(I+1) &" finished");      
    end Generic_Task;
    
    T_Arr: array(Range_P) of access Generic_Task;
    
  begin    
    for I in T_Arr'Range loop
      T_Arr(I) := new Generic_Task(I);
    end loop;
  end;
  
end P02;