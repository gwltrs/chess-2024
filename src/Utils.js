export function popup(str) {
    return function() {
        alert(str);
    };
}

export function timestamp() {
    return Math.round((new Date).getTime() / 1000);
}