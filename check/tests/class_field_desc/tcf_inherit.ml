class virtual parent = object
  val virtual v_virt : int
  val v_conc = 0
  method virtual m_virt : unit
  method m_conc = ()
end

class virtual c1 = object
  inherit parent
end

class virtual c2 = object
  method m_conc = ()
  inherit! parent
end

class virtual c3 = object
  inherit parent as p
end
