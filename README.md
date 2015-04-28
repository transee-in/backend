### transee backend

---

[![Build Status](https://img.shields.io/travis/transee-in/backend.svg)](https://travis-ci.org/transee-in/backend)

---

### deploy

* Install Erlang on your server
* Add authentication via ssh keys, `~/.ssh/config`:

```
Host transee
  HostName 127.0.0.1
  User transee
  IdentityFile ~/.ssh/id_rsa
```

* Clone this repo into `~/apps/transee`
* Exec

```bash
# for the first deploy on server
$ make get-deps compile release start-release

# for other deploys exec this on your local machine from app root
$ make deploy
```

* Setup nginx to listen `127.0.0.1:8000`

---

### contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request
