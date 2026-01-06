class A {
    A() {}
    boolean f() { return true; }
    boolean g() { return f();  }
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
	ok = ok && a.g() && ! b.g();

	a = b;
	b = (B)a;
	ok = ok && ! a.g() && ! b.g();

	a = new B();
	ok = ok && ! a.g();
	a = (A)a;
	ok = ok && ! a.g();

	if (ok)
	    System.out.print("ok\n");
    }
}
