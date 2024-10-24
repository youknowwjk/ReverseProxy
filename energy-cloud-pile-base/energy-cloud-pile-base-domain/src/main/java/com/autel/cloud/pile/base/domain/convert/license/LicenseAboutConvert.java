package com.autel.cloud.pile.base.domain.convert.license;

import com.autel.cloud.pile.base.dto.UnUsedLicenceInfoDto;
import com.autel.cloud.pile.base.enums.license.LicenceStatusEnum;
import com.autel.cloud.pile.base.enums.license.MeasureUnitEnum;
import com.autel.cloud.pile.base.enums.license.PileTypeEnum;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbLenBindRelationEntity;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

/**
 * 许可证相关的类型转换
 */
public class LicenseAboutConvert {

    private static final BigDecimal _1_DAT = new BigDecimal(String.valueOf(24 * 60 * 60 * 1000L));

    private LicenseAboutConvert() {

    }

    public static String getPileType(String chargeType) {

        if (PileTypeEnum.AC.getName().equalsIgnoreCase(chargeType)) {
            return PileTypeEnum.AC.getName();
        } else if (PileTypeEnum.DC.getName().equalsIgnoreCase(chargeType)) {
            return PileTypeEnum.DC.getName();
        } else {
            return "";
        }
    }

    public static Integer getMeasureUnit(String measureUnit) {

        if (MeasureUnitEnum.GUN.getName().equalsIgnoreCase(measureUnit)) {
            return MeasureUnitEnum.GUN.getCode();
        } else if (MeasureUnitEnum.PILE.getName().equalsIgnoreCase(measureUnit)) {
            return MeasureUnitEnum.PILE.getCode();
        } else {
            return null;
        }
    }

    public static Long getLastPurchaseDate(List<TbLenBindRelationEntity> tbLenBindRelationEntityList) {

        return tbLenBindRelationEntityList
                .stream()
                .map(TbLenBindRelationEntity::getCreateTime)
                .max(Long::compare)
                .orElse(null);
    }

    public static UnUsedLicenceInfoDto buildUnUsedLicenceInfoDto(TbLenBindRelationEntity unUsedTbLenBindRelationEntity) {

        UnUsedLicenceInfoDto unUsedLicenceInfoDto = new UnUsedLicenceInfoDto();
        BeanUtils.copyProperties(unUsedTbLenBindRelationEntity, unUsedLicenceInfoDto);
        unUsedLicenceInfoDto.setSubscribeDate(unUsedTbLenBindRelationEntity.getCreateTime());
        if ("NULL".equalsIgnoreCase(unUsedLicenceInfoDto.getChargeType())) {
            unUsedLicenceInfoDto.setChargeType(null);
        }
        unUsedLicenceInfoDto.setDaysRemaining(LicenseAboutConvert.getDaysRemaining(unUsedTbLenBindRelationEntity));
        return unUsedLicenceInfoDto;
    }

    public static List<UnUsedLicenceInfoDto> batchBuildUnUsedLicenceInfoDto(List<TbLenBindRelationEntity> unUsedTbLenBindRelationEntityList) {

        List<UnUsedLicenceInfoDto> unUsedLicenceInfoDtoList = new ArrayList<>();
        unUsedTbLenBindRelationEntityList.forEach(var -> unUsedLicenceInfoDtoList.add(LicenseAboutConvert.buildUnUsedLicenceInfoDto(var)));
        return unUsedLicenceInfoDtoList;
    }

    public static Integer getNaturalDays(long var1, long var2) {

        BigDecimal intervals = new BigDecimal(String.valueOf(var2)).subtract(new BigDecimal(String.valueOf(var1)));
        return Integer.valueOf(String.valueOf(intervals.divide(LicenseAboutConvert._1_DAT, 0, BigDecimal.ROUND_DOWN)));
    }

    public static Integer getDaysRemaining(TbLenBindRelationEntity tbLenBindRelationEntity) {

        final long currentTimeMillis = System.currentTimeMillis();
        if (LicenceStatusEnum.UNUSED.getCode().equals(tbLenBindRelationEntity.getStatus())) {
            if (tbLenBindRelationEntity.getUnavailableTime() != null) {
                return LicenseAboutConvert.getNaturalDays(currentTimeMillis, tbLenBindRelationEntity.getUnavailableTime());
            }
            Integer serviceTime = tbLenBindRelationEntity.getServiceTime();
            String timeUnit = tbLenBindRelationEntity.getTimeUnit();
            Calendar c = Calendar.getInstance();
            long var1 = c.getTimeInMillis();
            int unit = timeUnit.toLowerCase().startsWith("year") ? Calendar.YEAR : timeUnit.toLowerCase().startsWith("month") ? Calendar.MONTH : Calendar.DAY_OF_YEAR;
            c.add(unit, serviceTime);
            if (!StringUtils.isBlank(tbLenBindRelationEntity.getBonusDurationValue())) {
                String[] s = tbLenBindRelationEntity.getBonusDurationValue().split(" ");
                Integer bonusDurationTime = Integer.valueOf(s[0]);
                c.add(Calendar.MONTH,bonusDurationTime);
            }
            long var2 = c.getTimeInMillis();
            return LicenseAboutConvert.getNaturalDays(var1, var2);
        } else if ((LicenceStatusEnum.USED.getCode().equals(tbLenBindRelationEntity.getStatus())
                || Integer.valueOf(3).equals(tbLenBindRelationEntity.getStatus()))
                && (tbLenBindRelationEntity.getAvailableTime() != null
                        && tbLenBindRelationEntity.getUnavailableTime() != null
                        && tbLenBindRelationEntity.getUnavailableTime() > tbLenBindRelationEntity.getAvailableTime())) {
            final Long availableTime = tbLenBindRelationEntity.getAvailableTime();
             Long unavailableTime = tbLenBindRelationEntity.getUnavailableTime();
            if (!StringUtils.isBlank(tbLenBindRelationEntity.getBonusDurationValue())) {
                Calendar c = Calendar.getInstance();
                c.setTimeInMillis(unavailableTime);
                String[] s = tbLenBindRelationEntity.getBonusDurationValue().split(" ");
                Integer bonusDurationTime = Integer.valueOf(s[0]);
                c.add(Calendar.MONTH,bonusDurationTime);
                unavailableTime = c.getTimeInMillis();
            }
            if (availableTime < currentTimeMillis) {
                return LicenseAboutConvert.getNaturalDays(currentTimeMillis, unavailableTime);
            } else {
                return LicenseAboutConvert.getNaturalDays(availableTime, unavailableTime);
            }

        }
        return 0;
    }
}
