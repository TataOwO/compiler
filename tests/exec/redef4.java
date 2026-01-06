class A {
    int x;
    A() { }
}

class B extends A {
    int x;
    B() { x = 1;}
}

class Main {
    public static void main(String args[]) {
	A a = new A();
	B b = new B();
	boolean res = a.x == 0 && b.x == 1;
	a = b;
	if (res && a.x == 0)
	    System.out.print("ok\n");
    }
}
