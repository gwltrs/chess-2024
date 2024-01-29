export function popup(str) {
    return function() {
        alert(str);
    };
}

export function timeMS() {
    return new Date().getTime();
}

export function timeSec() {
    return Math.round(timeMS() / 1000);
}