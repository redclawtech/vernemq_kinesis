VerneMQ Kinesis Plugin
======================

This is a [VerneMQ](https://vernemq.com/) plugin that aggregates and sends MQTT messages to
[AWS Kinesis](https://aws.amazon.com/kinesis/).

### Prerequisites

* A recent version of Erlang OTP(19 recommended).

### How to compile it?

This project uses [Rebar3](https://www.rebar3.org) as a build tool and can be compiled running:

```bash
$ rebar3 compile
```

### How to enable the plugin on VerneMQ?

1. Manually enable:

```
vmq-admin plugin enable --name=vernemq_kinesis --path=<PathToYourPlugin>/vernemq_kinesis/_build/default/lib/vernemq_kinesis
```

2. Permanently enable (On Vernemq start):

Add the following to the *vernemq.conf* file.

```
plugins.vernemq_kinesis = on
plugins.vernemq_kinesis.path = <PathToYourPlugin>/vernemq_kinesis/_build/default/lib/vernemq_kinesis
```

### Configuration

