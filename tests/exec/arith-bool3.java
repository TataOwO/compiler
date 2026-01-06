class Main {
    public static void main(String args[]) {
	boolean bool1 = true && false || true  == ! false;   // true
	boolean bool2 = true && (false || true)  == ! false; // true
	boolean bool3 = true && ((false || false)  == ! false); // false

	boolean b = ! (bool1 && ! (bool2 || bool3)); // true

	if (b) System.out.print("ok\n");
    }
}
