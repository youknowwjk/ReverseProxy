package com.autel.cloud.pile.base.domain.repository.impl;

import com.autel.cloud.pile.base.domain.convert.DozerConvert;
import com.autel.cloud.pile.base.domain.repository.OpLocationEvaluationRepository;
import com.autel.cloud.pile.base.dto.OpLocationEvaluationDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationEvaluationMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEvaluationEntity;
import com.autel.cloud.pile.base.vo.OpLocationEvaluationVO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Objects;

@Service
@Log4j2
public class OplocationEvaluationRepositoryImpl extends ServiceImpl<OpLocationEvaluationMapper, OpLocationEvaluationEntity> implements OpLocationEvaluationRepository {
    @Autowired
    private OpLocationEvaluationMapper opLocationEvaluationMapper;

    /**
     * 场站评价分页
     *
     * @param opLocationEvaluationDTO 场站id
     * @return 场站评价分页
     */
    @Override
    public Page<OpLocationEvaluationVO> page(OpLocationEvaluationDTO opLocationEvaluationDTO) {
        Page<OpLocationEvaluationEntity> searchPage = new Page<>(opLocationEvaluationDTO.getPage(), opLocationEvaluationDTO.getPageSize());
        QueryWrapper<OpLocationEvaluationEntity> opLocationEvaluationEntityQueryWrapper = new QueryWrapper<>();
        opLocationEvaluationEntityQueryWrapper.eq("location_id", opLocationEvaluationDTO.getStationId());
        opLocationEvaluationEntityQueryWrapper.orderByDesc("time");
        Page<OpLocationEvaluationEntity> opLocationEvaluationEntityPage = opLocationEvaluationMapper.selectPage(searchPage, opLocationEvaluationEntityQueryWrapper);
        List<OpLocationEvaluationEntity> records = opLocationEvaluationEntityPage.getRecords();
        List<OpLocationEvaluationVO> opLocationEvaluationVOS = DozerConvert.mapList(records, OpLocationEvaluationVO.class);
        if (CollectionUtils.isNotEmpty(opLocationEvaluationVOS)) {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
            SimpleDateFormat timeSdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
            Date currentDate = new Date();
            String currentDateString = sdf.format(currentDate);
            for (OpLocationEvaluationVO opLocationEvaluationVO : opLocationEvaluationVOS) {
                String time = opLocationEvaluationVO.getTime();
                Date evaluationDate = new Date(Long.valueOf(time));
                String evaluationDateString = sdf.format(evaluationDate);
                log.info("evaluationDateString:{}", evaluationDateString);
                if (Objects.equals(currentDateString, evaluationDateString)) {
                    evaluationDateString = timeSdf.format(evaluationDate);
                    evaluationDateString = timeCalculate(evaluationDateString);

                }
                opLocationEvaluationVO.setTime(evaluationDateString);
            }
        }

        //封装结果集分页
        Page<OpLocationEvaluationVO> resultPage = new Page<>();
        resultPage.setCurrent(opLocationEvaluationEntityPage.getCurrent());
        resultPage.setSize(opLocationEvaluationEntityPage.getSize());
        resultPage.setTotal(opLocationEvaluationEntityPage.getTotal());
        resultPage.setRecords(opLocationEvaluationVOS);
        return resultPage;
    }

    private String timeCalculate(String time) {
        try {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
            String nowTimeStr = sdf.format(System.currentTimeMillis());
            //每天毫秒数
            long nd = 1000L * 24L * 60L * 60L;
            //每小时毫秒数
            long nh = 1000L * 60L * 60L;
            //每分钟毫秒数
            long nm = 1000L * 60L;

            Date nowDate = sdf.parse(nowTimeStr);
            Date date = sdf.parse(time);

            long diff = nowDate.getTime() - date.getTime();

            // 计算差多少天
            long day = diff / nd;
            // 计算差多少小时
            long hour = diff % nd / nh;
            // 计算差多少分钟
            long min = diff % nd % nh / nm;

            if (day == 0 && hour == 0 && min < 1) {
                return "刚刚";
            } else if (day == 0 && hour < 1) {
                return min + "分钟前";
            } else if (day < 1 && hour >= 1) {
                return hour + "小时前";
            } else {
                return day + "天前";
            }
        } catch (ParseException e) {
            return time;
        }
    }
}
