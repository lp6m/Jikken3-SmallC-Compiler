int W, H;
int* m[10];
int row0[10];
int row1[10];
int row2[10];
int row3[10];
int row4[10];
int inRange(int x, int y) {
    return 0 <= x && x < W && 0 <= y && y < H;
}
void set(int x, int y, int val) {
    int *p;
    if ( inRange(x, y) != 0 ) return;
    p = m[y];
    p[x] = val;
}
int get(int x, int y) {
    int *p;
    if ( inRange(x, y) != 0 ) return -1;
    p = m[y];
    return p[x];
}
void init() {
    m[0] = row0;
    m[1] = row1;
    m[2] = row2;
    m[3] = row3;
    m[4] = row4;
    set(3, 4, 1);
}
void search() {
    int x, y;
    for (y = 0; y < H; y = y + 1) {
        for (x = 0; x < W; x = x + 1) {
            if ( get(x, y) == 1 ) {
                print(x);
                print(y);
                return;
            }
        }
    }
    print(0);
}
int main() {
    W = 10, H = 5;
    init();
    search();
    return 0;
}