class A { int a; }

class B extends A { B() { } }

class Main {
    public static void main(String args[]) {
	if (new B().a == 0)
	    System.out.print("ok\n");
    }
}
