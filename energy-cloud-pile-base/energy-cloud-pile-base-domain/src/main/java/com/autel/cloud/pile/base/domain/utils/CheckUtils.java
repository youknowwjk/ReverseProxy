package com.autel.cloud.pile.base.domain.utils;

import java.math.BigDecimal;

public class CheckUtils {

    private CheckUtils() {
    }

    public static boolean checkLowerLimit(BigDecimal var1, BigDecimal var2, boolean equalLowerLimitMark) {
        return equalLowerLimitMark ? var2.compareTo(var1) >= 0 : var2.compareTo(var1) > 0;
    }

    public static boolean checkUpperLimit(BigDecimal var1, BigDecimal var2, boolean equalUpperLimitMark) {
        return equalUpperLimitMark ? var2.compareTo(var1) <= 0 : var2.compareTo(var1) < 0;
    }

    public static boolean checkIntervalLimit(BigDecimal var1, BigDecimal var2, BigDecimal var3, boolean equalLowerLimitMark, boolean equalUpperLimitMark) {
        return CheckUtils.checkLowerLimit(var1, var3, equalLowerLimitMark) && CheckUtils.checkUpperLimit(var2, var3, equalUpperLimitMark);
    }
}
