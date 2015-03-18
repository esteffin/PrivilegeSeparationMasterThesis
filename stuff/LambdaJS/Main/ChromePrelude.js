
"##Exercise#"

function Event()
{
    this.addListener = function (listener)
}

chrome = 
    {
        runtime: 
                {
                    sendMessage: function (msg, options, callback)
                    {
                        "##Exercise#Runtime"
                    },
                    onMessage: new Event()
                }
    }
