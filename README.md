# VerneMQ Kinesis Plugin

This is a [VerneMQ](https://vernemq.com/) plugin that aggregates and sends MQTT messages to
[AWS Kinesis](https://aws.amazon.com/kinesis/).

## Prerequisites

* A recent version of Erlang OTP(19 recommended).

## How to compile it?

This project uses [Rebar3](https://www.rebar3.org) as a build tool and can be compiled running:

```bash
$ rebar3 compile
```

## How to enable the plugin on VerneMQ

### Manually enable

```console
vmq-admin plugin enable --name=vernemq_kinesis --path=<PathToYourPlugin>/vernemq_kinesis/_build/default/lib/vernemq_kinesis
```

### Permanently enable (On VerneMQ start)

Add the following to the `vernemq.conf` file.

```
plugins.vernemq_kinesis = on
plugins.vernemq_kinesis.path = <PathToYourPlugin>/vernemq_kinesis/_build/default/lib/vernemq_kinesis
```

## Configuration

The following settings are available for this plugin:

    - **vernemq_kinesis.aws_key**: The AWS Key.
        See http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_access-keys.html.

    - **vernemq_kinesis.aws_secret_key**: The AWS secret key for connecting to Kinesis.
        See http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_access-keys.html.

    - **vernemq_kinesis.region**: The AWS region.

    - **vernemq_kinesis.stream**: The Kinesis stream name to write to.

    - **vernemq_kinesis.allowed_topics**: The topics that are allowed to publish to Kinesis.
        Can use "#" for all the topics or wilcards like <<"devices/#">>. Defaults to "#".

    - **vernemq_kinesis.batch_size**: The number of records to batch before flushing the queue.
        Defaults to 500.

    - **vernemq_kinesis.batch_time**: The maximum of milliseconds to wait before flushing the queue.
        Defaults to 20000(20 seconds).

They can be added to the `vernemq.conf` file.
