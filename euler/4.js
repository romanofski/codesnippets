function is_palindrome(item)
{
    var left = 0;
    var right = item.length - 1;
    while (left <= right) {
        if (item[left] != item[right]) {
            return false;
        }
        left++;
        right--;
    }
    return true;
}

function find_largest_palindrome()
{
    var x = 999;
    while (x >= 900) {
        y = 999;
        while (y >= 900) {
            prod = x * y;
            if (is_palindrome(prod.toString())) {
                print(prod);
            }
            y--;
        }
        x--;
    }

}
print(is_palindrome("9229"));
print(is_palindrome("922"));
print(find_largest_palindrome());
