package util;

public class MatrixWrapper<X>{
  private X[][] matrix;
  private int d1, d2;
  
  public MatrixWrapper (X[][] matrix) {
    this.matrix = matrix;
    d1 = matrix.length;
    d2 = d1 != 0 ? matrix[0].length : 0;
  }
  
  public int dim1() { return d1; }
  public int dim2() { return d2; }
  
  public X get(int i, int j){ return matrix[i][j]; }
  
}