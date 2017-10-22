package taiga.java.split.object;

import java.util.List;

import clojure.lang.IFn;
import clojure.lang.IFn.OD;

/** Can't make this Comparable, because we don't want a total ordering, which 
 * would make sorting more expensive. Java Comparable/Comparator may not sort
 * on partial orderings correctly, treating pairs of objects that compare as
 * equivalent. (This may only be a problem with sorted collections, not for 
 * sorting general lists, but the javadoc asserts that Comparable and Comparator 
 * must provide <em>total orderings</em>.
 * <par> 
 * TODO: Verify the sorting code here handles the partial ordering correctly.
 * 
 * @author John Alan McDonald
 * @since 2017-10-21
 */
@SuppressWarnings("unchecked") 
public final class XY {

  public final double x;
  public final Object y;
  //----------------------------------------------------------------------------
  // construction
  //----------------------------------------------------------------------------
  public XY (final double xx, final Object yy) {
    this.x = xx;
    this.y = yy; }
  //----------------------------------------------------------------------------
  // sorting
  //----------------------------------------------------------------------------
  private static final void swap (final XY[] xys,                   
                                  final int i,
                                  final int j) {
    final XY xyi = xys[i]; 
    xys[i] = xys[j]; 
    xys[j] = xyi; }
  //----------------------------------------------------------------------------
  private static final void swap (final XY[] xys,
                                  final int i0,
                                  final int i1,
                                  final int n) {
    int j = i0;
    int k = i1;
    for (int i=0;i<n;i++) { swap(xys, j, k); j++; k++; } }
  //----------------------------------------------------------------------------
  private static final int indexOfMedian (final XY[] xys,
                                          final int i,
                                          final int j,
                                          final int k) {
    final double xi = xys[i].x;
    final double xj = xys[j].x;
    final double xk = xys[k].x;
    if (Double.compare(xi,xj) < 0) {
      if (Double.compare(xj,xk) < 0) { return j; }
      if (Double.compare(xi,xk) < 0) { return k; }
      return i; }
    if (Double.compare(xj,xk) > 0) { return j; }
    if (Double.compare(xi,xk) > 0) { return k; }
    return i; }
  //----------------------------------------------------------------------------
  private static final void small (final XY[] xys, 
                                   final int start, 
                                   final int length) {
    final int end = start + length;
    for (int i=start; i<end; i++) {
      double x1 = xys[i].x;
      for (int j=i;j > start;j--) {
        final double x0 = xys[j-1].x;
        if (Double.compare(x0,x1)<0) { break; }
        x1 = x0;
        swap(xys,j,j-1); } } }
  //----------------------------------------------------------------------------
  private static final double initialPivot (final XY[] xys, 
                                            final int start, 
                                            final int length) {
    int i = start + (length >> 1);
    if (length > 7) {
      int i0 = start;
      int i1 = (start + length) - 1;
      if (length > 40) {
        final int s = length/8;
        i0 = indexOfMedian(xys, i0, i0+s, i0+(2*s));
        i = indexOfMedian(xys, i-s, i, i+s);
        i1 = indexOfMedian(xys, i1-(2*s), i1-s, i1); }
      i = indexOfMedian(xys, i0, i, i1); }
    return xys[i].x; }
  //---------------------------------------------------------------------------
  private static final void sort (final XY[] xys, 
                                  final int start, 
                                  final int length) {
    final int end = start + length;
    if (length < 7) { small(xys,start,length); return; }
    final int endm1 = end - 1;
    final double v = initialPivot(xys,start,length);
    int j0 = start;
    int j1 = start;
    int j2 = endm1;
    int j3 = endm1;
    for (;;j1++,j2--) {
      for (;j1<=j2;j1++) { 
        final double x1 = xys[j1].x;
        final int c = Double.compare(x1,v);
        if (c > 0) { break; }
        if (c == 0) { 
          swap(xys, j0, j1); 
          j0++; 
        }
      }
      for (;j2>=j1;j2--) {
        final double x2 = xys[j2].x;
        final int c = Double.compare(x2,v);
        if (c < 0) { break; }
        if (c == 0) { 
          swap(xys, j2, j3); 
          j3--;
        }
      }
      if (j1 > j2) { break; }
      swap(xys, j1, j2); 
    }
    
    final int k0 = Math.min(j0-start,j1-j0);
    swap(xys, start, j1-k0, k0);
    final int k1 = Math.min(j3-j2,end-j3-1);
    swap(xys, j1, end-k1, k1);
    final int k2 = j1-j0;
    if (k2 > 1) { sort(xys, start, k2); }
    final int k3 = j3-j2;
    if (k3 > 1) { sort(xys, end-k3, k3); } }
  //----------------------------------------------------------------------------
  public static final int notMissingX (final XY[] xys) {
    sort(xys,0,xys.length);
    for (int i=xys.length-1;i>=0;i--) {
      if (! Double.isNaN(xys[i].x)) { return i+1; } }
    throw new IllegalArgumentException("all xs are missing!"); }
  //----------------------------------------------------------------------------
  public static final Object[] cacheXY (final OD x, 
                                        final IFn y, 
                                        final List data,
                                        final XY[] xys) {
    final int n = data.size();
    assert n == xys.length;
    assert n > 0 : "No data!";
    for (int i=0;i<n;i++) {
      final Object di = data.get(i);

      final double xi = x.invokePrim(di);
      
      final Object yi = y.invoke(di);
      
      xys[i] = new XY(xi,yi); }
    
    final Integer notMissing = Integer.valueOf(notMissingX(xys));
    return new Object[] { xys, notMissing, }; }
  //----------------------------------------------------------------------------
}
