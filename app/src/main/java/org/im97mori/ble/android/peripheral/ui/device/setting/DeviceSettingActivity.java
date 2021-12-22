package org.im97mori.ble.android.peripheral.ui.device.setting;

import android.os.Bundle;
import android.text.TextUtils;
import android.view.MenuItem;
import android.view.View;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.fragment.app.FragmentTransaction;
import androidx.lifecycle.ViewModelProvider;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.databinding.DeviceSettingActivityBinding;
import org.im97mori.ble.android.peripheral.ui.BaseActivity;
import org.im97mori.ble.android.peripheral.utils.AfterTextChangedTextWatcher;
import org.im97mori.stacklog.LogUtils;

import dagger.hilt.android.AndroidEntryPoint;

@AndroidEntryPoint
public class DeviceSettingActivity extends BaseActivity {

    private DeviceSettingViewModel mViewModel;

    private DeviceSettingActivityBinding mBinding;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mViewModel = new ViewModelProvider(this).get(DeviceSettingViewModel.class);

        mBinding = DeviceSettingActivityBinding.inflate(getLayoutInflater());
        setContentView(mBinding.getRoot());

        mViewModel.observeDeviceTypeImageResId(this, mBinding.deviceTypeImage::setImageResource);
        mViewModel.observeDeviceTypeName(this, mBinding.deviceType::setText);
        mViewModel.observeDeviceSettingNameErrorString(this, s -> {
            mBinding.deviceName.setError(s);
            mBinding.deviceSettingCardView.setChecked(TextUtils.isEmpty(s));
        });
        mBinding.deviceNameEdit.addTextChangedListener(new AfterTextChangedTextWatcher(editable -> mViewModel.updateDeviceSettingName(editable)));
        mViewModel.observeDeviceSettingName(this, s -> distinctSetText(mBinding.deviceNameEdit, s));

        mBinding.topAppBar.setOnMenuItemClickListener(this::onOptionsItemSelected);

        mViewModel.observeFragmentReady(this, ready -> mBinding.rootContainer.setVisibility(ready ? View.VISIBLE : View.GONE));
        if (getSupportFragmentManager().findFragmentById(R.id.container) == null) {
            FragmentTransaction transaction = getSupportFragmentManager().beginTransaction();
            transaction.add(R.id.container, mViewModel.getFragment(getIntent()));
            transaction.commitNow();
        }
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
                    .subscribe(() -> {
                        setResult(RESULT_OK);
                        finish();
                    }, throwable -> Toast.makeText(this, throwable.getMessage(), Toast.LENGTH_SHORT).show()));
            result = true;
        } else {
            result = super.onOptionsItemSelected(item);
        }
        return result;
    }

}