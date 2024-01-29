export function popup(str) {
    return function() {
        alert(str);
    };
}

export function prettifyJSON(jsonStr) {

    // Modified version of:

    /*
        json-format v.1.1
        http://github.com/phoboslab/json-format

        Released under MIT license:
        http://www.opensource.org/licenses/mit-license.php
    */
    
    let json = jsonStr;
    
    let p = [];
    let out = "";
    let indent = 0;
    
    let push = (m) => '\\' + p.push(m) + '\\';
	let pop = (m, i) => p[i - 1];
	let tabs = (count) => new Array(count + 1).join('\t');

    json = json
        .replace(/\\./g, push)
        .replace(/(".*?"|'.*?')/g, push)
        .replace(/\s+/, '');		
    
    for (let i = 0; i < json.length; i++) {
        let c = json.charAt(i);
        switch(c) {
            case '{':
            case '[':
                out += c + "\n" + tabs(++indent);
                break;
            case '}':
            case ']':
                out += "\n" + tabs(--indent) + c;
                break;
            case ',':
                out += ",\n" + tabs(indent);
                break;
            case ':':
                out += ": ";
                break;
            default:
                out += c;
                break;      
        }					
    }

    out = out
        .replace(/\[[\d,\s]+?\]/g, (m) => m.replace(/\s/g,''))
        .replace(/\\(\d+)\\/g, pop)
        .replace(/\\(\d+)\\/g, pop);
    
    return out;
}

export function timeMS() {
    return new Date().getTime();
}

export function timeSec() {
    return Math.round(timeMS() / 1000);
}