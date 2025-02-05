var t = require('toy');

var r = class HWPlugin extends t.Plugin {
    render_our_now_eny(our, now, eny) {
        const message = `
<body>
    <p>Hello, ${our}.</p>
    <p>The time is ${now}</p>
    <p>Secret: ${eny}</p>
</body>
`;
        return message;
    }
}

module.exports = r;  //  not a real way to export things, TODO fix