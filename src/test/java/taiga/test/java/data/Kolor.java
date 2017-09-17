package taiga.test.java.data;

public enum Kolor { 
  RED, YELLOW, GREEN, CYAN, BLUE, MAGENTA; 

  @Override
  public final String toString () { 
    return "#taiga.test.java.data.Kolor \"" + name() + "\""; } }
