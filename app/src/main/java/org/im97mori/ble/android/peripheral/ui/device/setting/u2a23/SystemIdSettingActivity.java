package org.im97mori.ble.android.peripheral.ui.device.setting.u2a23;

import android.os.Bundle;
import android.view.MenuItem;
import android.view.View;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.ViewModelProvider;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.databinding.SystemIdSettingActivityBinding;
import org.im97mori.ble.android.peripheral.ui.BaseActivity;
import org.im97mori.ble.android.peripheral.utils.AfterTextChangedTextWatcher;
import org.im97mori.stacklog.LogUtils;

import dagger.hilt.android.AndroidEntryPoint;

@AndroidEntryPoint
public class SystemIdSettingActivity extends BaseActivity {

    private SystemIdSettingViewModel mViewModel;

    private SystemIdSettingActivityBinding mBinding;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mViewModel = new ViewModelProvider(this).get(SystemIdSettingViewModel.class);

        mBinding = SystemIdSettingActivityBinding.inflate(getLayoutInflater());
        setContentView(mBinding.getRoot());

        mViewModel.observeIsErrorResponse(this, check -> {
            mBinding.manufacturerIdentifier.setVisibility(check ? View.GONE : View.VISIBLE);
            mBinding.organizationallyUniqueIdentifier.setVisibility(check ? View.GONE : View.VISIBLE);
            mBinding.responseCode.setVisibility(check ? View.VISIBLE : View.GONE);
            mBinding.isErrorResponse.setChecked(check);
        });
        mBinding.isErrorResponse.setOnCheckedChangeListener((buttonView, isChecked) -> mViewModel.updateIsErrorResponse(isChecked));

        mViewModel.observeManufacturerIdentifier(this, charSequence -> distinctSetText(mBinding.manufacturerIdentifierEdit, charSequence));
        mViewModel.observeManufacturerIdentifierError(this, mBinding.manufacturerIdentifier::setError);
        mBinding.manufacturerIdentifierEdit.addTextChangedListener(new AfterTextChangedTextWatcher(editable
                -> mViewModel.updateManufacturerIdentifier(editable)));

        mViewModel.observeOrganizationallyUniqueIdentifier(this, charSequence -> distinctSetText(mBinding.organizationallyUniqueIdentifierEdit, charSequence));
        mViewModel.observeOrganizationallyUniqueIdentifierError(this, mBinding.organizationallyUniqueIdentifier::setError);
        mBinding.organizationallyUniqueIdentifierEdit.addTextChangedListener(new AfterTextChangedTextWatcher(editable
                -> mViewModel.updateOrganizationallyUniqueIdentifier(editable)));

        mViewModel.observeResponseCode(this, charSequence -> distinctSetText(mBinding.responseCodeEdit, charSequence));
        mViewModel.observeResponseCodeError(this, mBinding.responseCode::setError);
        mBinding.responseCodeEdit.addTextChangedListener(new AfterTextChangedTextWatcher(editable
                -> mViewModel.updateResponseCode(editable)));

        mViewModel.observeResponseDelay(this, charSequence -> distinctSetText(mBinding.responseDelayEdit, charSequence));
        mViewModel.observeResponseDelayError(this, mBinding.responseDelay::setError);
        mBinding.responseDelayEdit.addTextChangedListener(new AfterTextChangedTextWatcher(editable
                -> mViewModel.updateResponseDelay(editable)));

        mBinding.topAppBar.setOnMenuItemClickListener(this::onOptionsItemSelected);
    }

    @Override
    protected void onStart() {
        super.onStart();
        mDisposable.add(mViewModel.setup(getIntent())
                .subscribe(() -> mBinding.rootContainer.setVisibility(View.VISIBLE), throwable -> LogUtils.stackLog(throwable.getMessage())));
    }

    @Override
    public boolean onOptionsItemSelected(@NonNull MenuItem item) {
        boolean result;
        if (item.getItemId() == R.id.save) {
            mDisposable.add(mViewModel.save()
                    .subscribe(intent -> {
                        setResult(RESULT_OK, intent);
                        finish();
                    }, throwable -> Toast.makeText(this, throwable.getMessage(), Toast.LENGTH_SHORT).show()));
            result = true;
        } else {
            result = super.onOptionsItemSelected(item);
        }
        return result;
    }

}
