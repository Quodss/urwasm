var t = require('toy');  // import backend API definition
// define our own class
//
var r = class HWPlugin extends t.Plugin {
    render_our_now_eny(file, our, now, eny, dir, req) {
        if (req.method == "get") {
            return this.render_keep_color(file, our, now, eny);
        } else {
            return this.render_change_color(file, our, now, eny);
        }
    }

    render_keep_color(file, our, now, eny) {
        let config = t.get_config();
        let color;
        if (config == null) {
            this.config_set_default();
            color = "white";
        }
        else {
            color = config.color;
        }
        return this.build_message(file, our, now, eny, color);
    }

    render_change_color(file, our, now, eny) {
        let config = t.get_config();
        if (config == null) {
            config = this.config_set_default();
        }
        let color_number = 0;
        let i = eny.length - 1;
        let c = 0;
        while (true) {
            if (c == 5) break;
            let eny_char = eny[i];
            if (eny_char == '.') {
                i--;
                continue;
            }
            else {
                let eny_dig;
                if (eny_char >= '0' && eny_char <= '9') {
                    eny_dig = eny_char.charCodeAt(0) - '0'.charCodeAt(0);
                }
                else {
                    eny_dig = eny_char.charCodeAt(0) - 'a'.charCodeAt(0) + 10;
                }
                color_number = color_number * 32 + eny_dig;
                i--;
                c++;
            }
        }
        color_number >>>= 1;  // 25 bits to 24 bits
        const hex_color = `#${color_number.toString(16).padStart(6, "0")}`;
        config.color = hex_color;
        t.set_config(config);
        return this.build_message(file, our, now, eny, hex_color);
    }

    config_set_default() {
        let config = {"color": "white"};
        t.set_config(config);
        return config;
    }

    build_message(file, our, now, eny, color) {
        const message = `
<html>
  <head>
    <style>
    body {background: ${color};}
    </style>
  </head>
  <body>
    <p>${file}</p>
    <p>Hello, ${our}.</p>
    <p>The time is ${now}</p>
    <p>Secret: ${eny}</p>
    <p>Color: ${color}</p>
    <br>
    <br>
    <form method="POST">
      <button type="submit">Change Color</button>
    </form>
  </body>
</html>
`;
        return message;
    }
}
//
module.exports = r; // export our class