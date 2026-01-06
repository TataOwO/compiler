class A { int a; }

class B extends A { B() { a = 1; } }

class Main {
    public static void main(String args[]) {
	if (new B().a == 1)
	    System.out.print("ok\n");
    }
}
