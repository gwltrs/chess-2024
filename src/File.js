export function saveFile(name) {
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

export function loadFile_(just) {
    return function(nothing) {
        return function() {
            return new Promise((res, rej) => {
                console.log('loadFile_ begin');
                const fileInput = document.getElementById('fileInput');
                fileInput.addEventListener('change', () => {
                    console.log('loadFile_ change', fileInput.files);
                    const file = fileInput.files[0];
                    const fileReader = new FileReader();
                    fileReader.onload = (e) => {
                        const text = fileReader.result;
                        res(just(text));
                    };
                    if (file instanceof Blob) {
                        fileReader.readAsText(file);
                    }
                });
                fileInput.addEventListener('cancel', () => {
                    console.log('loadFile_ cancel');
                    res(nothing);
                });
                fileInput.click();
            });
        }
    }
}