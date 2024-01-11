export function saveFile (name) {
    return function (text) {
        return function () {
            const element = document.createElement('a');
            element.setAttribute('href', 'data:text/plain;charset=utf-8,' + encodeURIComponent(text));
            element.setAttribute('download', name);
            if (document.createEvent) {
                const event = document.createEvent('MouseEvents');
                event.initEvent('click', true, true);
                element.dispatchEvent(event);
            } else {
                element.click();
            }
        };
    };
};