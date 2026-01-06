class A {
    int x;
    int y;
    A() {}
    void f() {
        System.out.print(x + "\n");
        System.out.print(y + "\n");
    }
}

class B extends A {
    B(int x, int y) { this.x = x; this.y =y; }
    void f() {
        System.out.print((x+1) + "\n");
        System.out.print((y+1) + "\n");
    }
}

class Main {
    public static void main(String args[]) {
	A a = new B(17, 42);
        a.f();
    }
}
