class A {
    A() {}
    boolean f() { return true; }
}

class B extends A {
    B() {}
    boolean f() { return false; }
}

class Main {
    public static void main(String args[]) {
	boolean ok = true;

	A a = new A();
	B b = new B();
	ok = ok && a.f() && ! b.f();

	a = b;
	b = (B)a;
	ok = ok && ! a.f() && ! b.f();

	a = new B();
	ok = ok && ! a.f();
	a = (A)a;
	ok = ok && ! a.f();

	if (ok)
	    System.out.print("ok\n");
    }
}
