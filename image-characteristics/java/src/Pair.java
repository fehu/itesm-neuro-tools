package util;

public class Pair<X, Y> { 
    public final X x; 
    public final Y y; 
    public Pair(X x, Y y) { 
      this.x = x; 
      this.y = y; 
    } 
    
    public X _1(){ return x; }
    public Y _2(){ return y; }
  } 