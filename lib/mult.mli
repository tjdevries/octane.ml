(* Copyright (C) 2017--2018  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * and the LGPL-3.0 Linking Exception along with this library.  If not, see
 * <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.
 *)

(** Row multiplicity. *)

type +'m t constraint 'm = [< `Zero | `One | `Many]

type zero = [`Zero]
type one = [`One]
type zero_or_one = [`Zero | `One]
type zero_or_more = [`Zero | `One | `Many]

val zero : [> `Zero] t
val one : [> `One] t
val zero_or_one : [> `Zero | `One] t
val zero_or_more : [> `Zero | `One | `Many] t

val only_zero : [< `Zero] t -> unit
val only_one : [< `One] t -> unit
val only_zero_or_one : [< `Zero | `One] t -> unit

val expose : 'm t -> [`Zero | `One | `Zero_or_one | `Zero_or_more]

val can_be_zero : 'm t -> bool
val can_be_many : 'm t -> bool
