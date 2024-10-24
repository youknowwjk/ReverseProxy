package com.autel.cloud.pile.base.domain.utils;

import lombok.extern.slf4j.Slf4j;

import java.util.regex.Pattern;

@Slf4j
public class CheckUtil {
    public static boolean checkEmail(String email) {
        if (email == null){
            return false;
        }
        String pattern = "[a-zA-Z0-9._-]+@[a-zA-Z0-9.-]+.[a-zA-Z]{2,6}";
        if (Pattern.matches(pattern, email)) {
            return true;
        }
        return false;
    }
}
