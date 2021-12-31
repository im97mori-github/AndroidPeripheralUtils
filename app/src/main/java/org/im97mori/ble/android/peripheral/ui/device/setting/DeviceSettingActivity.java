package org.im97mori.ble.android.peripheral.ui.device.setting;

import static org.im97mori.ble.android.peripheral.utils.Utils.setTextDistinct;

import android.os.Bundle;
import android.text.TextUtils;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.view.MenuProvider;
import androidx.fragment.app.FragmentTransaction;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.databinding.DeviceSettingActivityBinding;
import org.im97mori.ble.android.peripheral.ui.device.setting.fragment.BaseSettingFragmentViewModel;
import org.im97mori.ble.android.peripheral.utils.AfterTextChangedTextWatcher;
import org.im97mori.ble.android.peripheral.utils.MockitoViewModelProvider;
import org.im97mori.stacklog.LogUtils;

import dagger.hilt.android.AndroidEntryPoint;

@AndroidEntryPoint
public class DeviceSettingActivity extends AppCompatActivity {

    private DeviceSettingViewModel mViewModel;

    private DeviceSettingActivityBinding mBinding;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mViewModel = new MockitoViewModelProvider(this).get(DeviceSettingViewModel.class);

        mBinding = DeviceSettingActivityBinding.inflate(getLayoutInflater());
        setContentView(mBinding.getRoot());

        mViewModel.observeDeviceTypeImageResId(this, mBinding.deviceTypeImage::setImageResource);
        mViewModel.observeDeviceTypeName(this, mBinding.deviceType::setText);
        mViewModel.observeDeviceSettingNameErrorString(this, s -> {
            mBinding.deviceSettingName.setError(s);
            mBinding.deviceSettingCardView.setChecked(TextUtils.isEmpty(s));
        });
        mBinding.deviceSettingNameEdit.addTextChangedListener(new AfterTextChangedTextWatcher(editable -> mViewModel.updateDeviceSettingName(editable)));
        mViewModel.observeDeviceSettingName(this, s -> setTextDistinct(mBinding.deviceSettingNameEdit, s));

        mBinding.topAppBar.addMenuProvider(new MenuProvider() {

            @Override
            public void onCreateMenu(@NonNull Menu menu, @NonNull MenuInflater menuInflater) {
                menu.findItem(R.id.save).setEnabled(mBinding.rootContainer.getVisibility() == View.VISIBLE);
            }

            @Override
            public boolean onMenuItemSelected(@NonNull MenuItem menuItem) {
                boolean result = false;
                if (menuItem.getItemId() == R.id.save) {
                    BaseSettingFragmentViewModel fragmentViewModel = new MockitoViewModelProvider(DeviceSettingActivity.this).get(mViewModel.getFragmentViewModelClass(getIntent()));
                    mViewModel.observeSave(fragmentViewModel::getModuleDataString, () -> {
                        setResult(RESULT_OK);
                        finish();
                    }, throwable -> Toast.makeText(DeviceSettingActivity.this, throwable.getMessage(), Toast.LENGTH_SHORT).show());
                    result = true;
                }
                return result;
            }
        });
        mBinding.topAppBar.setOnMenuItemClickListener(this::onOptionsItemSelected);

        mViewModel.observeFragmentReady(this, ready -> {
            mBinding.rootContainer.setVisibility(ready ? View.VISIBLE : View.GONE);
            mBinding.topAppBar.invalidateMenu();
        });
        if (getSupportFragmentManager().findFragmentById(R.id.fragmentContainer) == null) {
            FragmentTransaction transaction = getSupportFragmentManager().beginTransaction();
            transaction.add(R.id.fragmentContainer, mViewModel.getFragment(getIntent()));
            transaction.commit();
        }
    }

    @Override
    protected void onStart() {
        super.onStart();
        mViewModel.observeSetup(getIntent(), () -> {
        }, throwable -> LogUtils.stackLog(throwable.getMessage()));
    }

}