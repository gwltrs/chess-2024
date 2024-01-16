export function loadFile_(just) {
    return function(nothing) {
        return function() {
            return new Promise((res, rej) => {
                const fileInput = document.getElementById('fileInput');
                fileInput.addEventListener('change', () => {
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
                    res(nothing);
                });
                fileInput.click();
            });
        };
    };
}

export function saveFile(name) {
    return function (text) {
        return function () {
            const element = document.createElement('a');
            element.setAttribute('href', 'data:text/plain;charset=utf-8,' + encodeURIComponent(text));
            element.setAttribute('download', name);
            element.click();
        };
    };
};