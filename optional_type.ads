--stolen from wiki https://en.wikipedia.org/wiki/Option_type#Ada
generic
  type E_Type is private;

Package Optional_Type is
  -- When the discriminant, Has_Element, is true there is an element field,
  -- when it is false, there are no fields (hence the null keyword).
  Type Optional( Has_Element : Boolean:= False ) is record
    case Has_Element is
      when False => null;
      when True  => Element : E_Type;
    end case;
  end record;

end Optional_Type;

