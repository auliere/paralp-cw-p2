---------------------------------------------------------
------------------PARALLEL PROGRAMMING-------------------
-------------------COURSE WORK PART 2--------------------
----       --ADA. SEMAPHORES. PROTECTED UNITS--     -----
----              MA = MB*MC + MO*ME*a              -----
---------------------------------------------------------
-------------     MAIN MODULE PRG1     ------------------
---------------------------------------------------------
-----------------CREATED ON 20.03.2016-------------------
----------------BY OLEG PEDORENKO, IP-31-----------------
---------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Synchronous_Task_Control; use Ada.Synchronous_Task_Control;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;
with Ada.Real_Time;      use Ada.Real_Time;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Data;


procedure P02 is
  N: Integer := 8;
  P: Integer := 2;
  H: Integer := 4;
-- t_start - start time; t_copy - copy start time; t_calc - calc start time; t_finish - finish time
  t_start, t_copy, t_finish: Ada.Real_Time.Time;
  t_calc: Ada.Real_Time.Time := Ada.Real_Time.Time_First;  
  
  procedure Write_Time_To_File(
    Start_Time, End_Time: Ada.Real_Time.Time;
    File_Name: String; Comment: String := "") is
    Work_Time: Duration;
    My_File: Ada.Text_IO.File_type;
  begin
    Work_Time := Ada.Real_Time.To_Duration(End_Time - Start_Time);
    Put("N =   "); Put(N);
    Put(Duration'Image(Work_Time)); Put("s. "); Put_Line("(" & Comment & ")");
    
    Create (File => My_File, Mode => Out_File, 
      Name => File_Name);
    Put(File => My_File, 
      Item => Trim(Duration'Image(Work_Time), 
                Ada.Strings.Left));
    Put(File => My_File, Item => ";");
    Close(My_File);  
  end;
  
begin
  --Read N and P from command line
  if(Argument_Count > 0) then 
    N := Integer'Value(Argument(1));
  end if;  
  if(Argument_Count > 1) then 
    P := Integer'Value(Argument(2));
  end if;  
  H := Integer(Float'Ceiling(Float(N)/Float(P)));
  
  declare    
    -- Types
    package Data_S is new Data(N, 100); use Data_S;
    subtype Range_P is Integer range 1..(P);
    -- Variables
    MA, MB, MC, MO, ME: aliased Matrix;
    a: aliased Scalar;    
    --SEMAPHORE
    Sem: array(Range_P) of Suspension_Object;
    
    -- Protected module for shared resource
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
        Read_Allowed := False;
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
    
    -- Calculate MAh
    procedure Calculate
      (MBa, MCa, MOa, MEa: access Matrix; aa: access Scalar; Q: Integer) is
      Sum1, Sum2: Scalar;
      B, E: Integer;
    begin
      B := (H*Q + 1);
      E := (H*(Q+1));
      if (Q = (P - 1)) then
        E := N;
      end if;
      for I in 1..N loop
        for J in B..E loop
          Sum1 := 0.0;
          Sum2 := 0.0;
          for K in 1..N loop
            Sum1 := Sum1 + (MBa.all(I)(K) * MCa.all(J)(K)); --MB * MCh
            Sum2 := Sum2 + (MOa.all(I)(K) * MEa.all(J)(K)); -- MO * MEh
          end loop;
          MA(J)(I) := Sum1 + Sum2 * aa.all;
        end loop;
      end loop;
    end;
    
    
    -- GENERIC TASK T1 or TX --
    task type Generic_Task(I: Integer) is
      pragma Storage_Size(100000000);
    end Generic_Task;

    task body Generic_Task is
      MB1, MO1: aliased Matrix;
      a1: aliased Scalar;      
    begin
      Put_Line("Task T" & Integer'Image(I) &" started");
      --For task T1
      if (I = 1) then
        --Initialize MA
        Fill(MA);
        --Enter MB, MC, MO, ME, a
        Put_Line("MB =   "); Input(MB); 
        Put_Line("MC =   "); Input(MC); 
        Put_Line("MO =   "); Input(MO); 
        Put_Line("ME =   "); Input(ME);      
        Put_Line("a =    "); Input(a); Put(a); Put_Line("");
        MB(2)(2) := 3.0;
        t_copy := Clock;
        --Signal SN,1
        Shared_Vars.Write(MB, MO, a);      
      end if;      
      --Wait W1,1
      --Copy MB, MO, a
      Shared_Vars.Read(MB1, MO1, a1);
      --Calculate MAh
      if(t_calc = Ada.Real_Time.Time_First) then
        t_calc := Clock;
      end if;
      Calculate(MB1'Access, MC'Access, MO1'Access, ME'Access, a1'Access, I - 1);      
      --Signal S1,1
      Set_True(Sem(I));      
      --For task T1
      if (I = 1) then
        --Wait WN,1
        for I in Sem'Range loop
          Suspend_Until_True(Sem(I));
        end loop;
        --Output MA
        Put_Line("MA =   "); Output(MA);
        
        t_finish := Clock;
        Write_Time_To_File(t_start, t_finish, 
          Trim(Integer'Image(N), Ada.Strings.Left) & "-all",
          "Total work time");
        Write_Time_To_File(t_copy, t_finish, 
          Trim(Integer'Image(N), Ada.Strings.Left) & "-copy_calc",
          "Copy and calculation time");
        Write_Time_To_File(t_calc, t_finish, 
          Trim(Integer'Image(N), Ada.Strings.Left) & "-calc",
          "Calculation time");
        Put_Line("Task T1 finished");     
      end if;
      
      Put_Line("Task T" & Integer'Image(I) &" finished");      
    end Generic_Task;
    
    
    T_Arr: array(Range_P) of access Generic_Task;
  
  -- Generate P tasks
  begin   
    t_start := Clock;
    
    for I in T_Arr'Range loop
      T_Arr(I) := new Generic_Task(I);
    end loop;    
  end;
  
end P02;