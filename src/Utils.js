export function popup(str) {
    return function() {
        alert(str);
    };
}