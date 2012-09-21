#
# Copyright 2012 Elovalo project group 
# 
# This file is part of Elovalo.
# 
# Elovalo is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# Elovalo is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Elovalo.  If not, see <http:#www.gnu.org/licenses/>.
#

import collections

# Returns an immutable namedtuple with the keys and values of the given tuple
# Useful for providing config options
def ConfigTuple(name, dic):
    conftuple = collections.namedtuple(name, dic.keys())
    ct = conftuple(**dic)
    return ct
