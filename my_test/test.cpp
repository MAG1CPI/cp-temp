#include <iostream>

int main() {
    int a[4][3][2] = {1, 2, {}, {}, {{3}}, 4};
    for (int i = 0; i < 4; i++)
        for (int j = 0; j < 3; j++) {
            for (int k = 0; k < 2; k++)
                std::cout << a[i][j][k] << " ";
            std::cout << "\n";
        }
    return 0;
}
