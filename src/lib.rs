//! Adds a functionality to easily convert between [toml_datetime]'s and
//! [chrono](::chrono)'s/[time](::time)'s types.
//!
//! # Features
//! - `chrono` enables [chrono](::chrono) conversions
//! - `time` enables [time](::time) conversions
//! - `serde_with` enables [`TomlDateTime`] to use with [serde_with](::serde_with)
//!
//! # Using [`serde`] derive macros
//! This crate can be used with
//! [`#[serde(with="toml_datetime_compat")]`](https://serde.rs/field-attrs.html#with),
//! but the functions [`deserialize`] and [`serialize`] can also be used on
//! their own to (de)serialize [`chrono`](::chrono) an [`time`](::time) types.
//!
//! Meaning this struct
//! ```
//! # use serde::{Deserialize, Serialize};
//! #[derive(Deserialize, Serialize)]
//! struct SomeDateTimes {
#![cfg_attr(
    feature = "chrono",
    doc = r#"
    #[serde(with = "toml_datetime_compat")]
    chrono_naive_date: chrono::NaiveDate,
    #[serde(with = "toml_datetime_compat")]
    chrono_naive_time: chrono::NaiveTime,
    #[serde(with = "toml_datetime_compat")]
    chrono_naive_date_time: chrono::NaiveDateTime,
    #[serde(with = "toml_datetime_compat")]
    chrono_date_time_utc: chrono::DateTime<chrono::Utc>,
    #[serde(with = "toml_datetime_compat")]
    chrono_date_time_offset: chrono::DateTime<chrono::FixedOffset>,"#
)]
#![cfg_attr(
    feature = "time",
    doc = r#"
    #[serde(with = "toml_datetime_compat")]
    time_date: time::Date,
    #[serde(with = "toml_datetime_compat")]
    time_time: time::Time,
    #[serde(with = "toml_datetime_compat")]
    time_primitive_date_time: time::PrimitiveDateTime,
    #[serde(with = "toml_datetime_compat")]
    time_offset_date_time: time::OffsetDateTime,"#
)]
//! }
//! ```
//! will (de)serialize from/to
//! ```toml
#![cfg_attr(
    feature = "time",
    doc = r"naive_date = 1523-08-20
naive_time = 23:54:33.000011235
naive_date_time = 1523-08-20T23:54:33.000011235
date_time_utc = 1523-08-20T23:54:33.000011235Z
date_time_offset = 1523-08-20T23:54:33.000011235+04:30"
)]
#![cfg_attr(
    feature = "time",
doc = r"date = 1523-08-20
time = 23:54:33.000011235
primitive_date_time = 1523-08-20T23:54:33.000011235
offset_date_time = 1523-08-20T23:54:33.000011235+04:30")]
//! ```
//!
#![cfg_attr(
    feature = "time",
    doc = r"# Using [serde_with](::serde_with)

It is also possible to use [serde_with](::serde_with) using the [`TomlDateTime`]
converter.

This is especially helpful to deserialize optional date time values (due to
[serde-rs/serde#723](https://github.com/serde-rs/serde/issues/723)).

")]
//!
//! # Using [`FromToTomlDateTime`]
//!
//! And by introducing a new trait [`FromToTomlDateTime`] that adds
//! [`to_toml`](FromToTomlDateTime::to_toml) and
//! [`from_toml`](FromToTomlDateTime::from_toml) functions to the relevant
//! structs from [`chrono`](::chrono) and [`time`](::time).
#![warn(clippy::pedantic, missing_docs)]
#![allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
use std::result::Result as StdResult;

use serde::{de::Error as _, ser::Error as _, Deserialize, Deserializer, Serialize, Serializer};
use toml_datetime::Datetime as TomlDatetime;
#[cfg(any(feature = "chrono", feature = "time"))]
use toml_datetime::{Date as TomlDate, Offset as TomlOffset, Time as TomlTime};

#[cfg(feature = "serde_with")]
pub use crate::serde_with::TomlDateTime;

/// Function that can be used with
/// [`#[serde(deserialize_with="toml_datetime_compat::deserialize")]`](https://serde.rs/field-attrs.html#deserialize_with)
#[allow(clippy::missing_errors_doc)]
pub fn deserialize<'de, D: Deserializer<'de>, T: FromToTomlDateTime>(
    deserializer: D,
) -> StdResult<T, D::Error> {
    FromToTomlDateTime::from_toml(TomlDatetime::deserialize(deserializer)?)
        .map_err(D::Error::custom)
}

/// Function that can be used with
/// [`#[serde(serialize_with="toml_datetime_compat::serialize")]`](https://serde.rs/field-attrs.html#serialize_with)
#[allow(clippy::missing_errors_doc)]
pub fn serialize<S: Serializer, T: FromToTomlDateTime>(
    value: &T,
    serializer: S,
) -> StdResult<S::Ok, S::Error> {
    value
        .to_toml()
        .map_err(S::Error::custom)?
        .serialize(serializer)
}

#[cfg(feature = "serde_with")]
mod serde_with {
    use serde::{Deserializer, Serializer};
    use serde_with::{DeserializeAs, SerializeAs};

    use crate::FromToTomlDateTime;

    /// Struct to allow the integration into the [`serde_with`](::serde_with)
    /// ecosystem
    #[cfg_attr(any(feature = "time", feature = "chrono"), doc = r#"```
# use serde::{Deserialize, Serialize};
use serde_with::serde_as;

#[serde_as]
#[derive(Serialize, Deserialize)]
struct OptionalDateTimes {
    #[serde_as(as = "Option<toml_datetime_compat::TomlDateTime>")]"#)]
    #[cfg_attr(feature = "time", doc = "    value: Option<time::Date>")]
    #[cfg_attr(
        all(not(feature = "time"),
        feature = "chrono"),
        doc = "    value: Option<chrono::NaiveDate>"
    )]
    #[cfg_attr(any(feature = "time", feature = "chrono"), doc = "}
```")]
    pub struct TomlDateTime;

    impl<'de, T: FromToTomlDateTime> DeserializeAs<'de, T> for TomlDateTime {
        fn deserialize_as<D: Deserializer<'de>>(deserializer: D) -> Result<T, D::Error> {
            crate::deserialize(deserializer)
        }
    }
    impl<T: FromToTomlDateTime> SerializeAs<T> for TomlDateTime {
        fn serialize_as<S: Serializer>(source: &T, serializer: S) -> Result<S::Ok, S::Error> {
            crate::serialize(source, serializer)
        }
    }
}

/// Error that can occur while transforming [`TomlDatetime`] from and to
/// [`chrono`](::chrono) and [`time`](::time) types
#[derive(thiserror::Error, Debug)]
pub enum Error {
    /// Caused by years that cannot be represented in [`TomlDate::year`]
    #[error("year out of range for toml")]
    InvalidYear,
    /// Caused by converting a [`TomlDatetime`] without a date to a type
    /// requiring a date component
    #[error("expected date")]
    ExpectedDate,
    /// Caused by converting a [`TomlDatetime`] with a date to a type
    /// without a date component
    #[error("unexpected date")]
    UnexpectedDate,
    /// Caused by converting a [`TomlDatetime`] without a time to a type
    /// requiring a time component
    #[error("expected time")]
    ExpectedTime,
    /// Caused by converting a [`TomlDatetime`] with a time to a type
    /// without a time component
    #[error("unexpected time")]
    UnexpectedTime,
    /// Caused by converting a [`TomlDatetime`] without a time zone to a type
    /// requiring a time zone component
    #[error("expected time zone")]
    ExpectedTimeZone,
    /// Caused by converting a [`TomlDatetime`] with a time zone to a type
    /// without a time zone component
    #[error("unexpected offset")]
    UnexpectedTimeZone,
    /// Caused by converting a [`TomlDatetime`] without the UTC time zone to a
    /// type requiring UTC time zone
    #[error("expected UTC date time (either `Z` or +00:00)")]
    ExpectedUtcTimeZone,
    /// Creating rust type failed due to the date time parsed by
    /// [`TomlDatetime`] being invalid
    ///
    /// [`toml_datetime`] should already validate this
    #[error("unable to create rust type from toml type")]
    UnableToCreateRustType,
}

type Result<T> = StdResult<T, Error>;

/// Trait that allows easy conversion between [`TomlDatetime`] and
/// [`chrono`'s](::chrono)/[`time`'s](::time) types
pub trait FromToTomlDateTime: Sized {
    /// Converts from a [`TomlDatetime`]
    ///
    /// # Errors
    /// Fails when the [`TomlDatetime`] contains data not representable by
    /// [`Self`] or is missing data required by [`Self`]
    fn from_toml(value: TomlDatetime) -> Result<Self>;
    /// Converts to a [`TomlDatetime`]
    ///
    /// # Errors
    /// Fails when the [`Self`] is not representable by [`TomlDatetime`] mainly
    /// due to a negative year
    fn to_toml(&self) -> Result<TomlDatetime>;
}

#[cfg(feature = "chrono")]
mod chrono {
    use chrono::{
        DateTime, Datelike, Duration, FixedOffset, NaiveDate, NaiveDateTime, NaiveTime, Offset,
        Timelike, Utc,
    };

    use crate::{Error, FromToTomlDateTime, Result, TomlDate, TomlDatetime, TomlOffset, TomlTime};

    impl FromToTomlDateTime for NaiveDate {
        fn from_toml(TomlDatetime { date, time, offset }: TomlDatetime) -> Result<Self> {
            if time.is_some() {
                return Err(Error::UnexpectedTime);
            }
            if offset.is_some() {
                return Err(Error::UnexpectedTimeZone);
            }
            let TomlDate { year, month, day } = date.ok_or(Error::ExpectedDate)?;
            NaiveDate::from_ymd_opt(year.into(), month.into(), day.into())
                .ok_or(Error::UnableToCreateRustType)
        }

        fn to_toml(&self) -> Result<TomlDatetime> {
            Ok(TomlDatetime {
                date: Some(TomlDate {
                    year: self.year().try_into().map_err(|_| Error::InvalidYear)?,
                    month: self.month() as u8,
                    day: self.day() as u8,
                }),
                time: None,
                offset: None,
            })
        }
    }

    impl FromToTomlDateTime for NaiveTime {
        fn from_toml(TomlDatetime { date, time, offset }: TomlDatetime) -> Result<Self> {
            if date.is_some() {
                return Err(Error::UnexpectedDate);
            }
            if offset.is_some() {
                return Err(Error::UnexpectedTimeZone);
            }
            let TomlTime {
                hour,
                minute,
                second,
                nanosecond,
            } = time.ok_or(Error::ExpectedTime)?;
            NaiveTime::from_hms_nano_opt(hour.into(), minute.into(), second.into(), nanosecond)
                .ok_or(Error::UnableToCreateRustType)
        }

        fn to_toml(&self) -> Result<TomlDatetime> {
            Ok(TomlDatetime {
                date: None,
                time: Some(TomlTime {
                    hour: self.hour() as u8,
                    minute: self.minute() as u8,
                    second: self.second() as u8,
                    nanosecond: self.nanosecond(),
                }),
                offset: None,
            })
        }
    }

    impl FromToTomlDateTime for NaiveDateTime {
        fn from_toml(TomlDatetime { date, time, offset }: TomlDatetime) -> Result<Self> {
            let date = NaiveDate::from_toml(TomlDatetime {
                date,
                time: None,
                offset,
            })?;
            Ok(if time.is_some() {
                NaiveDateTime::new(
                    date,
                    NaiveTime::from_toml(TomlDatetime {
                        date: None,
                        time,
                        offset,
                    })?,
                )
            } else {
                date.and_hms_opt(0, 0, 0).expect("00:00:00 is a valid time")
            })
        }

        fn to_toml(&self) -> Result<TomlDatetime> {
            Ok(TomlDatetime {
                date: self.date().to_toml()?.date,
                time: self.time().to_toml()?.time,
                offset: None,
            })
        }
    }

    impl FromToTomlDateTime for DateTime<Utc> {
        fn from_toml(TomlDatetime { date, time, offset }: TomlDatetime) -> Result<Self> {
            match offset {
                Some(
                    TomlOffset::Z
                    | TomlOffset::Custom {
                        hours: 0,
                        minutes: 0,
                    },
                ) => {
                    let date = NaiveDateTime::from_toml(TomlDatetime {
                        date,
                        time,
                        offset: None,
                    })?;
                    Ok(DateTime::from_utc(date, Utc))
                }
                _ => Err(Error::ExpectedUtcTimeZone),
            }
        }

        fn to_toml(&self) -> Result<TomlDatetime> {
            let date_time = self.naive_local().to_toml()?;
            Ok(TomlDatetime {
                offset: Some(TomlOffset::Z),
                ..date_time
            })
        }
    }

    impl FromToTomlDateTime for DateTime<FixedOffset> {
        fn from_toml(TomlDatetime { date, time, offset }: TomlDatetime) -> Result<Self> {
            match offset {
                Some(offset) => {
                    let date = NaiveDateTime::from_toml(TomlDatetime {
                        date,
                        time,
                        offset: None,
                    })?;
                    Ok(DateTime::from_local(date, match offset {
                        TomlOffset::Z => {
                            FixedOffset::east_opt(0).expect("00:00 is a valid time zone offset")
                        }
                        TomlOffset::Custom { hours, minutes } => FixedOffset::east_opt(
                            i32::from(hours) * 60 * 60
                                + i32::from(minutes)
                                    * 60
                                    * if hours.is_positive() { 1 } else { -1 },
                        )
                        .ok_or(Error::UnableToCreateRustType)?,
                    }))
                }
                _ => Err(Error::ExpectedTimeZone),
            }
        }

        fn to_toml(&self) -> Result<TomlDatetime> {
            let timezone = Duration::seconds(self.timezone().fix().local_minus_utc().into());
            let hours = timezone.num_hours();
            let minutes = timezone.num_minutes() - hours * 60;
            let date_time = self.naive_local().to_toml()?;
            Ok(TomlDatetime {
                offset: Some(TomlOffset::Custom {
                    hours: hours as i8,
                    minutes: minutes as u8,
                }),
                ..date_time
            })
        }
    }
}

#[cfg(feature = "time")]
mod time {
    use time::{error::ComponentRange, Date, OffsetDateTime, PrimitiveDateTime, Time, UtcOffset};

    use crate::{Error, FromToTomlDateTime, Result, TomlDate, TomlDatetime, TomlOffset, TomlTime};

    impl From<ComponentRange> for Error {
        fn from(_: ComponentRange) -> Self {
            Self::UnableToCreateRustType
        }
    }

    impl FromToTomlDateTime for Date {
        fn from_toml(TomlDatetime { date, time, offset }: TomlDatetime) -> Result<Self> {
            if time.is_some() {
                return Err(Error::UnexpectedTime);
            }
            if offset.is_some() {
                return Err(Error::UnexpectedTimeZone);
            }
            let TomlDate { year, month, day } = date.ok_or(Error::ExpectedDate)?;
            Date::from_calendar_date(year.into(), month.try_into()?, day).map_err(From::from)
        }

        fn to_toml(&self) -> Result<TomlDatetime> {
            Ok(TomlDatetime {
                date: Some(TomlDate {
                    year: self.year().try_into().map_err(|_| Error::InvalidYear)?,
                    month: self.month() as u8,
                    day: self.day(),
                }),
                time: None,
                offset: None,
            })
        }
    }

    impl FromToTomlDateTime for Time {
        fn from_toml(TomlDatetime { date, time, offset }: TomlDatetime) -> Result<Self> {
            if date.is_some() {
                return Err(Error::UnexpectedDate);
            }
            if offset.is_some() {
                return Err(Error::UnexpectedTimeZone);
            }
            let TomlTime {
                hour,
                minute,
                second,
                nanosecond,
            } = time.ok_or(Error::ExpectedTime)?;
            Time::from_hms_nano(hour, minute, second, nanosecond).map_err(From::from)
        }

        fn to_toml(&self) -> Result<TomlDatetime> {
            Ok(TomlDatetime {
                date: None,
                time: Some(TomlTime {
                    hour: self.hour(),
                    minute: self.minute(),
                    second: self.second(),
                    nanosecond: self.nanosecond(),
                }),
                offset: None,
            })
        }
    }

    impl FromToTomlDateTime for PrimitiveDateTime {
        fn from_toml(TomlDatetime { date, time, offset }: TomlDatetime) -> Result<Self> {
            let date = Date::from_toml(TomlDatetime {
                date,
                time: None,
                offset,
            })?;
            Ok(if time.is_some() {
                PrimitiveDateTime::new(
                    date,
                    Time::from_toml(TomlDatetime {
                        date: None,
                        time,
                        offset,
                    })?,
                )
            } else {
                date.midnight()
            })
        }

        fn to_toml(&self) -> Result<TomlDatetime> {
            Ok(TomlDatetime {
                date: self.date().to_toml()?.date,
                time: self.time().to_toml()?.time,
                offset: None,
            })
        }
    }

    impl FromToTomlDateTime for OffsetDateTime {
        fn from_toml(TomlDatetime { date, time, offset }: TomlDatetime) -> Result<Self> {
            match offset {
                Some(offset) => {
                    let date = PrimitiveDateTime::from_toml(TomlDatetime {
                        date,
                        time,
                        offset: None,
                    })?;
                    Ok(date.assume_offset(match offset {
                        TomlOffset::Z => UtcOffset::UTC,
                        TomlOffset::Custom { hours, minutes } => UtcOffset::from_hms(
                            hours,
                            minutes
                                .try_into()
                                .map_err(|_| Error::UnableToCreateRustType)?,
                            0,
                        )
                        .map_err(|_| Error::UnableToCreateRustType)?,
                    }))
                }
                _ => Err(Error::ExpectedTimeZone),
            }
        }

        fn to_toml(&self) -> Result<TomlDatetime> {
            Ok(TomlDatetime {
                date: self.date().to_toml()?.date,
                time: self.time().to_toml()?.time,
                offset: Some(TomlOffset::Custom {
                    hours: self.offset().whole_hours(),
                    minutes: self.offset().minutes_past_hour().unsigned_abs(),
                }),
            })
        }
    }
}

#[test]
#[cfg(feature = "chrono")]
fn chrono() {
    use ::chrono::{DateTime, FixedOffset, NaiveDate, NaiveDateTime, NaiveTime, Utc};
    use indoc::formatdoc;
    use pretty_assertions::assert_eq;
    use serde::{Deserialize, Serialize};

    const Y: i32 = 1523;
    const M: u32 = 8;
    const D: u32 = 20;
    const H: u32 = 23;
    const MIN: u32 = 54;
    const S: u32 = 33;
    const NS: u32 = 11_235;
    const OH: i32 = 4;
    const OM: i32 = 30;

    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct Test {
        #[serde(with = "crate")]
        naive_date: NaiveDate,
        #[serde(with = "crate")]
        naive_time: NaiveTime,
        #[serde(with = "crate")]
        naive_date_time: NaiveDateTime,
        #[serde(with = "crate")]
        date_time_utc: DateTime<Utc>,
        #[serde(with = "crate")]
        date_time_offset: DateTime<FixedOffset>,
    }

    let naive_date = NaiveDate::from_ymd_opt(Y, M, D).unwrap();
    let naive_time = NaiveTime::from_hms_nano_opt(H, MIN, S, NS).unwrap();
    let naive_date_time = NaiveDateTime::new(naive_date, naive_time);

    let input = Test {
        naive_date,
        naive_time,
        naive_date_time,
        date_time_utc: DateTime::from_utc(naive_date_time, Utc),
        date_time_offset: DateTime::from_local(
            naive_date_time,
            FixedOffset::east_opt((OH * 60 + OM) * 60).unwrap(),
        ),
    };

    let serialized = toml::to_string(&input).unwrap();

    assert_eq!(
        serialized,
        dbg!(formatdoc! {"
            naive_date = {Y:04}-{M:02}-{D:02}
            naive_time = {H:02}:{MIN:02}:{S:02}.{NS:09}
            naive_date_time = {Y:04}-{M:02}-{D:02}T{H:02}:{MIN:02}:{S:02}.{NS:09}
            date_time_utc = {Y:04}-{M:02}-{D:02}T{H:02}:{MIN:02}:{S:02}.{NS:09}Z
            date_time_offset = {Y:04}-{M:02}-{D:02}T{H:02}:{MIN:02}:{S:02}.{NS:09}+{OH:02}:{OM:02}
            "})
    );

    assert_eq!(toml::from_str::<Test>(&serialized).unwrap(), input);
}

#[cfg(all(feature = "time", test))]
mod time_test {
    use ::time::{Date, Month, OffsetDateTime, PrimitiveDateTime, Time, UtcOffset};
    use indoc::formatdoc;
    use pretty_assertions::assert_eq;
    use serde::{Deserialize, Serialize};

    const Y: i32 = 1523;
    const M: u8 = 8;
    const D: u8 = 20;
    const H: u8 = 23;
    const MIN: u8 = 54;
    const S: u8 = 33;
    const NS: u32 = 11_235;
    const OH: i8 = 4;
    const OM: i8 = 30;

    #[test]
    fn time() {
        #[derive(Serialize, Deserialize, Debug, PartialEq)]
        struct Test {
            #[serde(with = "crate")]
            date: Date,
            #[serde(with = "crate")]
            time: Time,
            #[serde(with = "crate")]
            primitive_date_time: PrimitiveDateTime,
            #[serde(with = "crate")]
            offset_date_time: OffsetDateTime,
        }

        let date = Date::from_calendar_date(Y, Month::try_from(M).unwrap(), D).unwrap();
        let time = Time::from_hms_nano(H, MIN, S, NS).unwrap();
        let primitive_date_time = PrimitiveDateTime::new(date, time);

        let input = Test {
            date,
            time,
            primitive_date_time,
            offset_date_time: primitive_date_time
                .assume_offset(UtcOffset::from_hms(OH, OM, 0).unwrap()),
        };

        let serialized = toml::to_string(&input).unwrap();

        assert_eq!(
            serialized,
            dbg!(formatdoc! {"
            date = {Y:04}-{M:02}-{D:02}
            time = {H:02}:{MIN:02}:{S:02}.{NS:09}
            primitive_date_time = {Y:04}-{M:02}-{D:02}T{H:02}:{MIN:02}:{S:02}.{NS:09}
            offset_date_time = {Y:04}-{M:02}-{D:02}T{H:02}:{MIN:02}:{S:02}.{NS:09}+{OH:02}:{OM:02}
            "})
        );

        assert_eq!(toml::from_str::<Test>(&serialized).unwrap(), input);
    }

    #[test]
    #[cfg(feature = "serde_with")]
    fn serde_with() {
        use serde_with::serde_as;

        use crate::TomlDateTime;

        #[serde_as]
        #[derive(Serialize, Deserialize, Debug, PartialEq)]
        struct Test {
            #[serde_as(as = "Option<TomlDateTime>")]
            optional_date_time: Option<OffsetDateTime>,
        }

        let input = Test {
            optional_date_time: Some(
                PrimitiveDateTime::new(
                    Date::from_calendar_date(Y, Month::try_from(M).unwrap(), D).unwrap(),
                    Time::from_hms_nano(H, MIN, S, NS).unwrap(),
                )
                .assume_offset(UtcOffset::from_hms(OH, OM, 0).unwrap()),
            ),
        };

        let serialized = toml::to_string(&input).unwrap();

        assert_eq!(
            serialized,
            dbg!(formatdoc! {"
            optional_date_time = {Y:04}-{M:02}-{D:02}T{H:02}:{MIN:02}:{S:02}.{NS:09}+{OH:02}:{OM:02}
            "})
        );

        assert_eq!(toml::from_str::<Test>(&serialized).unwrap(), input);

        let input = Test {
            optional_date_time: None,
        };

        let serialized = toml::to_string(&input).unwrap();

        assert!(serialized.trim().is_empty());

        assert_eq!(toml::from_str::<Test>(&serialized).unwrap(), input);
    }
}
