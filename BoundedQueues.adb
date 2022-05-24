--with Optional_Type;
package body BoundedQueues is
   -- package O is new Optional_Type(Element_Type => Element_Type);
   --use O;
   function Make return Queue is
     (First => 0, Size => 0, content => (others => (Has_Element => False)));

   function GetContent (Q : Queue) return QueueContent is (Q.content);
   function GetFirst (Q : Queue) return Natural is (Q.First);
   function GetLast (Q : Queue) return Natural is
     (if (Q.Size = 0) then Q.First else Q.First + (Q.Size - 1));
   --function GetDefault (Q : Queue) return Element_Type is (Q.default);

   function Consistent (Q : Queue) return Boolean is
     (((Q.First in Q.content'Range) and
       (Q.Size = 0 or Q.First + Q.Size - 1 <= Q.content'Last))
      and then
      ((for all I in Q.content'First .. Q.First - 1 =>
          not Q.content (I).Has_Element) and
       (for all I in Q.First .. (Q.First + Q.Size) - 1 =>
          Q.content (I).Has_Element) and
       (for all I in (Q.First + Q.Size) .. Q.content'Last =>
          not Q.content (I).Has_Element)));
   --  ((Q.First in Q.content'Range) and
   --
   --   ((Q.Size = 0 and
   --     (for all I in Q.content'Range => not Q.content (I).Has_Element)) or
   --    (Q.Size = 1 and
   --     (for all I in Q.content'Range =>
   --        ((I = Q.First and Q.content (I).Has_Element)
   --         or else not Q.content (I).Has_Element))) or
   --    ((Q.First + Q.Size - 1 <= Q.content'Last)
   --     and then
   --     ((for all I in Q.content'First .. Q.First - 1 =>
   --         not Q.content (I).Has_Element) and
   --      (for all I in Q.First .. (Q.First + Q.Size) - 1 =>
   --         Q.content (I).Has_Element) and
   --      (for all I in (Q.First + Q.Size) .. Q.content'Last =>
   --                            not Q.content (I).Has_Element)))
   --       )
   --  );

   --  function Consistent (Q : Queue) return Boolean is
   --  begin
   --     --first check for empty and early return
   --     --  if (Q.Size = 0) then
   --     --     return True;
   --     --  end if;
   --
   --     --then check if fields match
   --     if (Q.First not in Q.content'Range) or (Q.Size not in Q.content'Range) or
   --       (Q.First + Q.Size - 1 not in Q.content'Range)
   --     then
   --        return False;
   --     else
   --
   --        -- check in front
   --        for I in Q.content'First .. Q.First - 1 loop
   --           if Q.content (I).Has_Element then
   --              return False;
   --           end if;
   --           pragma Loop_Invariant (I in Q.content'Range);
   --           pragma Loop_Invariant
   --             (for all J in Q.content'First .. I =>
   --                not Q.content (J).Has_Element);
   --
   --        end loop;
   --        --check middle
   --        for I in Q.First .. Q.First + Q.Size - 1 loop
   --           if not Q.content (I).Has_Element then
   --              return False;
   --           end if;
   --           pragma Loop_Invariant (I in Q.content'Range);
   --           pragma Loop_Invariant
   --             (for all J in Q.content'First .. Q.First - 1 =>
   --                not Q.content (J).Has_Element);
   --           pragma Loop_Invariant
   --             (for all J in Q.First .. I => Q.content (J).Has_Element);
   --
   --        end loop;
   --
   --        --check in rear
   --        for I in Q.First + Q.Size .. Q.content'Last loop
   --           if Q.content (I).Has_Element then
   --              return False;
   --           end if;
   --           pragma Loop_Invariant (I in Q.content'Range);
   --           pragma Loop_Invariant
   --             (for all J in Q.content'First .. Q.First - 1 =>
   --                not Q.content (J).Has_Element);
   --           pragma Loop_Invariant
   --             (for all J in Q.First .. Q.First + Q.Size - 1 =>
   --                Q.content (J).Has_Element);
   --           pragma Loop_Invariant
   --             (for all J in Q.First + Q.Size .. I =>
   --                not Q.content (J).Has_Element);
   --
   --        end loop;
   --     end if;
   --     return True;
   --  end Consistent;

   function GetSize (Q : Queue) return Natural is (Q.Size);
   function IsEmpty (Q : Queue) return Boolean is (Q.Size = 0);
   function IsFull (Q : Queue) return Boolean is
     (GetLast (Q) = Q.content'Length - 1);
   procedure Pop (Q : in out Queue; e : out Element_Type) is
   --  tmp: Optional;
   begin

      if Q.Size < 1 then
         raise Underflow;
      end if;
      pragma Assert (Consistent (Q));
      --precondition makes sure we have an element
      e := Q.content (Q.First).Element;

      --   case tmp.Has_Element is
      --     when False => raise Inconsistent;
      --     when True => e := tmp.Element;
      --  end case;
      Q.Size              := Q.Size - 1;
      Q.content (Q.First) := (Has_Element => False);
      if Q.First /= Q.content'Last then
         Q.First := Q.First + 1;
      end if;
      pragma Assert (Consistent (Q));
   end Pop;

   procedure Push (Q : in out Queue; e : in Element_Type) is
   begin
      if GetLast (Q) > Q.content'Last then
         raise Overflow;
      end if;
      if (IsEmpty (Q)) then
         Q.content (GetFirst (Q)) := (True, e);
      else
         Q.content (GetLast (Q) + 1) := (True, e);
      end if;
      Q.Size := Q.Size + 1;
   end Push;

   function GetElements (Q : Queue) return Element_Array is
      tmp     : Element_Array (Q.content'First .. (GetSize (Q) - 1));
      current : Natural range tmp'Range := Q.content'First;
   begin
      if IsEmpty (Q) then
         tmp := (others => <>);
         return tmp;
      else

         for I in Q.content'Range loop
            pragma Loop_Invariant (current <= I);
            pragma Loop_Invariant (current in tmp'Range);

            if Q.content (I).Has_Element then
               tmp (current) := Q.content (I).Element;
               if current < tmp'Last then
                  current := current + 1;
               end if;
            end if;

         end loop;
      end if;
      pragma Assert (GetSize (Q) = tmp'Length);
      return tmp;
   end GetElements;

end BoundedQueues;
