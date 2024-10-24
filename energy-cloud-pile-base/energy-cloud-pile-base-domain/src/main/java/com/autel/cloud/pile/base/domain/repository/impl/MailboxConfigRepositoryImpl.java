package com.autel.cloud.pile.base.domain.repository.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.collection.CollUtil;
import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.pile.base.domain.repository.MailboxConfigRepository;
import com.autel.cloud.pile.base.dto.MailboxConfigDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.TbMailboxConfigMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbMailboxConfigEntity;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.IdWorker;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.log4j.Log4j2;
import org.apache.http.util.Asserts;
import org.springframework.stereotype.Service;

import java.util.Date;

@Service
@Log4j2
public class MailboxConfigRepositoryImpl extends ServiceImpl<TbMailboxConfigMapper, TbMailboxConfigEntity> implements MailboxConfigRepository {
    @Override
    public Boolean addMailbox(MailboxConfigDTO mailboxConfigDTO) {
        Asserts.check(!isExistedMailbox(mailboxConfigDTO), "该运营商已配置邮箱，不能重复配置");
        TbMailboxConfigEntity tbMailboxConfigEntity = new TbMailboxConfigEntity();
        BeanUtil.copyProperties(mailboxConfigDTO, tbMailboxConfigEntity);
        tbMailboxConfigEntity.setId(IdWorker.getId(TbMailboxConfigEntity.class));
        tbMailboxConfigEntity.setCreateId(0L);
        tbMailboxConfigEntity.setCreateTime(new Date());
        return save(tbMailboxConfigEntity);
    }

    @Override
    public Boolean updateMailbox(MailboxConfigDTO mailboxConfigDTO) {
        Asserts.check(!isExistedMailbox(mailboxConfigDTO), "该运营商已配置邮箱，不能重复配置");
        TbMailboxConfigEntity tbMailboxConfigEntity = new TbMailboxConfigEntity();
        BeanUtil.copyProperties(mailboxConfigDTO, tbMailboxConfigEntity);
        tbMailboxConfigEntity.setId(IdWorker.getId(TbMailboxConfigEntity.class));
        return updateById(tbMailboxConfigEntity);
    }

    @Override
    public Boolean delMailbox(Long id) {
        Asserts.check(null == id, "参数为空");
        return removeById(id);
    }

    @Override
    public PageVO<MailboxConfigDTO> selectMailboxPage(MailboxConfigDTO mailboxConfigDTO) {
        return null;
    }

    @Override
    public TbMailboxConfigEntity selectMailboxByOperatorId(Long operatorId) {
        Asserts.check(null != operatorId, "参数为空");
        LambdaQueryWrapper<TbMailboxConfigEntity> queryWrapper = Wrappers.lambdaQuery(TbMailboxConfigEntity.class);
        queryWrapper.eq(TbMailboxConfigEntity::getOperatorId, operatorId);
        queryWrapper.eq(TbMailboxConfigEntity::getDeleted, 0);
        return getOne(queryWrapper);
    }

    private Boolean isExistedMailbox(MailboxConfigDTO mailboxConfigDTO) {
        LambdaQueryWrapper<TbMailboxConfigEntity> queryWrapper = Wrappers.lambdaQuery(TbMailboxConfigEntity.class);
        queryWrapper.eq(TbMailboxConfigEntity::getOperatorId, mailboxConfigDTO.getOperatorId());
        queryWrapper.eq(TbMailboxConfigEntity::getDeleted, 0);
        queryWrapper.ne(null != mailboxConfigDTO.getId(), TbMailboxConfigEntity::getId, mailboxConfigDTO.getId());
        return CollUtil.isNotEmpty(list(queryWrapper));
    }
}
