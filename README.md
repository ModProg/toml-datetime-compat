# toml-datetime-compat

Adds a functionality to easily convert between `toml_datetime`'s and
`chrono`'s/`time`'s types.

Both with the `serde` derive macros:

```rust
#[derive(Deserialize, Serialize)]
struct SomeDateTimes {
    #[serde(with = "toml_datetime_compat")]
    chrono_naive_date: chrono::NaiveDate,
    #[serde(with = "toml_datetime_compat")]
    chrono_naive_time: chrono::NaiveTime,
    #[serde(with = "toml_datetime_compat")]
    chrono_naive_date_time: chrono::NaiveDateTime,
    #[serde(with = "toml_datetime_compat")]
    chrono_date_time_utc: chrono::DateTime<chrono::Utc>,
    #[serde(with = "toml_datetime_compat")]
    chrono_date_time_offset: chrono::DateTime<chrono::FixedOffset>,
    #[serde(with = "toml_datetime_compat")]
    time_date: time::Date,
    #[serde(with = "toml_datetime_compat")]
    time_time: time::Time,
    #[serde(with = "toml_datetime_compat")]
    time_primitive_date_time: time::PrimitiveDateTime,
    #[serde(with = "toml_datetime_compat")]
    time_offset_date_time: time::OffsetDateTime,"#
}
```

And by introducing a new trait `ToFromToml` that adds `to_toml` and `from_toml`
functions to the relevant structs from `chrono` and `time`.
