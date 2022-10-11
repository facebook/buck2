// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use crate::{inline::InlineString, SmartStringMode};

pub(crate) enum StringCast<'a, Mode: SmartStringMode> {
    Boxed(&'a Mode::BoxedString),
    Inline(&'a InlineString),
}

pub(crate) enum StringCastMut<'a, Mode: SmartStringMode> {
    Boxed(&'a mut Mode::BoxedString),
    Inline(&'a mut InlineString),
}

pub(crate) enum StringCastInto<Mode: SmartStringMode> {
    Boxed(Mode::BoxedString),
    Inline(InlineString),
}
