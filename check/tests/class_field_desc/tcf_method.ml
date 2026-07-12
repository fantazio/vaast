class p = object
  method m_over = 0
end

class virtual c = object
  inherit p
  method m_conc = 0
  method virtual m_virt : int
  method private m_priv = 1
  method! m_over = 2
end

