package com.autel.cloud.pile.base.infrastructure.util;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

public class JudgeTimeUtils {
    /**
     * 判断时间是否在时间段内
     */
    public static boolean belongCalendar(Date nowTime, Date beginTime, Date endTime) {
        Calendar date = Calendar.getInstance();
        date.setTime(nowTime);
        Calendar begin = Calendar.getInstance();
        begin.setTime(beginTime);

        Calendar end = Calendar.getInstance();
        end.setTime(endTime);
        if(date.compareTo(begin)<0&&date.before(end)){
            date.add(Calendar.DATE,1);
        }
        if(end.before(begin)){
            end.add(Calendar.DATE,1);
        }
        return date.compareTo(begin)>=0 && date.compareTo(end)<=0;
    }
    /**
     * 比较一个 HH:mm 是否在一个时间段内
     * 如：14:33 是否在 09:30 和 12:00 内
     */
    public static boolean timeIsInRound(String str1, String start, String end) {
        SimpleDateFormat df = new SimpleDateFormat("HH:mm");
        Date now = null;
        Date beginTime = null;
        Date endTime = null;
        if (null == start || null == end) {
            return true;
        } else {
            try {
                now = df.parse(str1);
                beginTime = df.parse(start);
                endTime = df.parse(end);
            } catch (Exception e) {
                e.printStackTrace();
            }
            return belongCalendar(now, beginTime, endTime);
        }
    }



}
