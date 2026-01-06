class Main {
    public static void main(String args[]) {
	boolean bool1 =   1 < 2 && 2 <= 3 || (1 + 3)  == (4 + 0); // true
	boolean bool2 = 2 > 1 && (2 >= 3 || 3 != 3)  == ! (3 == 4); // true
	boolean bool3 = - 2 * - 5 ==  2 && 1 == 2 && 0 == 0; // false

	boolean b = ! (bool1 && ! (bool2 || bool3)); // true

	if (! b) System.out.print("ok\n");
    }
}
