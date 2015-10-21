mod_apns
====
An ejabberd module to send offline messages as PUSH notifications for iOS.

> Consider using [mod_push](https://github.com/royneary/mod_push) which implements [XEP-0357](http://xmpp.org/extensions/xep-0357.html) and works with many PUSH services.

This module **has nothing to do** with [XEP-0357](http://xmpp.org/extensions/xep-0357.html) so far.

The main goal of this module is to send all offline messages to the registered (see [Usage](#Usage)) clients via APNS.

**Compilation**:

Because of the dependencies such as xml.hrl, logger.hrl, etc it's recommended to compile the module with ejabberd itself: put it in the *ejabberd/src* directory and run the default compiler.

**Configuration**:

To let the module work fine with Apple Push Notification Service APIs, put these lines in the modules section:

```yaml
mod_apns:
  address: "gateway.push.apple.com"
  port: 2195
  certfile: "cert.pem"
  keyfile: "key.pem"
```
You can use a *password* field in case if you have a password-protected certificate.

**<a name="Usage"></a>Usage (Client to server)**:

You need to send this stanza to the server over the XMPP connection, to let the server know your client token:
```xml
<iq to="YourServer" type="set">
  <register xmlns="https://apple.com/push" >
    <token>TOKEN</token>
  </register>
</iq>
```

The key will be kept in mnesia database and completely belongs to the JabberID which it was sent from.

**What it sends to APNS**:
```json
{
  "aps":
    {
      "alert": "Hello!",
      "sound": "default"
    }, 
  "source": "user@example.com",
  "destination":"user2@example.com"
}
```

**Compatibility**:

Module should be working fine with Ejabberd 14+.
