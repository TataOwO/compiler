
// Pascal triangle modulo 7

class List {
    int val;
    List next;

    List(int n) {
        if (n == 1) return;
        next = new List(n-1);
    }

    int get(int i) {
        if (i == 0) return val;
        return next.get(i-1);
    }

    void set(int i, int v) {
        if (i == 0) val = v;
        else next.set(i-1, v);
    }

}

class Pascal {

    int h;

    Pascal(int h) { this.h = h; }

    void print_row(List r, int i) {
        int j;
        for (j = 0; j <= i; j=j+1) {
            if (r.get(j) != 0)
                System.out.print("*");
            else
                System.out.print("0");
        }
        System.out.print("\n");
    }

    void compute_row(List r, int j) {
        int v = 0;
        if (j == 0)
            v = 1;
        else
            v = (r.get(j) + r.get(j-1)) % 7;
        r.set(j, v);
        if (j > 0)
            compute_row(r, j-1);
    }

    void run() {
        List r = new List(h+1);
        int i;
        for (i = 0; i < h; i=i+1) {
            r.set(i, 0);
            compute_row(r, i);
            print_row(r, i);
        }
    }
}

class Main {
    public static void main(String args[]) {
        new Pascal(42).run();
    }
}
